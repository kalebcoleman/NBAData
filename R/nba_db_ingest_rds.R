#' Ingest parsed RDS tables into SQLite with upsert
#'
#' @param seasons Integer vector of seasons
#' @param parsed_dir Directory containing parsed RDS files
#' @param db_path SQLite database path
#' @param warn_only Emit warnings instead of stopping on validation issues
#' @param validate Validate parsed tables before ingest when TRUE
#' @return A list of per-season ingest results
#' @export
nba_db_ingest_rds <- function(seasons,
                              parsed_dir = "data/parsed",
                              db_path = "data/sql/nbadata.sqlite",
                              warn_only = TRUE,
                              validate = FALSE) {
  seasons <- as.integer(seasons)
  results <- list()

  con <- nba_db_connect(db = db_path, drv = "sqlite")
  on.exit(nba_db_disconnect(con), add = TRUE)
  nba_db_init(con, target_version = NBA_SCHEMA_VERSION)

  for (season in seasons) {
    started_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ingest_id <- sprintf("%s-rds-%s", season, format(Sys.time(), "%Y%m%d%H%M%S"))
    out <- list(ok = TRUE, error = NA_character_, n_games = 0L, n_team_box = 0L, n_player_box = 0L)

    tryCatch(
      {
        path <- file.path(parsed_dir, sprintf("parsed_%s.rds", season))
        if (!file.exists(path)) {
          stop(sprintf("Parsed RDS not found: %s", path))
        }
        tables <- readRDS(path)
        out$n_games <- nrow(tables$games)
        out$n_team_box <- nrow(tables$team_box)
        out$n_player_box <- nrow(tables$player_box)
        message(sprintf(
          "Loaded %s (games=%s, team_box=%s, player_box=%s)",
          path,
          out$n_games,
          out$n_team_box,
          out$n_player_box
        ))
        if (isTRUE(validate)) {
          validate_parsed_tables(tables, warn_only = warn_only)
        }
        write_parsed_tables(
          tables,
          format = "sqlite",
          mode = "upsert",
          con = con
        )
      },
      error = function(e) {
        out$ok <- FALSE
        out$error <- conditionMessage(e)
      }
    )

    finished_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    DBI::dbExecute(
      con,
      "INSERT INTO nbadata_ingest_log
       (ingest_id, season, season_type, started_at, finished_at,
        n_games, n_team_box, n_player_box, source, ok, error)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        ingest_id,
        as.integer(season),
        NA_character_,
        started_at,
        finished_at,
        as.integer(out$n_games),
        as.integer(out$n_team_box),
        as.integer(out$n_player_box),
        "rds",
        as.integer(out$ok),
        out$error
      )
    )

    results[[as.character(season)]] <- out
  }

  results
}
