#' Connect to NBAData database
#'
#' @param db Database path (sqlite) or connection string (postgres)
#' @param drv One of "sqlite" or "postgres"
#' @param ... Additional DBI connection args
#' @return A DBI connection
#' @export
nba_db_connect <- function(db = "data/sql/nbadata.sqlite",
                           drv = c("sqlite", "postgres"),
                           ...) {
  drv <- match.arg(drv)
  if (drv == "sqlite") {
    return(DBI::dbConnect(RSQLite::SQLite(), db, ...))
  }

  stop("Postgres support not implemented. TODO: add RPostgres backend.", call. = FALSE)
}

#' Disconnect from NBAData database
#'
#' @param con DBI connection
#' @return TRUE invisibly
#' @export
nba_db_disconnect <- function(con) {
  DBI::dbDisconnect(con)
  invisible(TRUE)
}

#' Initialize NBAData database schema
#'
#' @param con DBI connection
#' @param target_version Schema version to migrate to
#' @return TRUE invisibly
#' @export
nba_db_init <- function(con, target_version = NBA_SCHEMA_VERSION) {
  .nba_db_create_meta_tables(con)
  current <- nba_db_get_version(con)
  if (current < target_version) {
    nba_db_apply_migrations(con, current, target_version)
  }
  .nba_db_create_indices(con)
  invisible(TRUE)
}

.nba_db_create_meta_tables <- function(con) {
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS nbadata_meta (
       schema_version INTEGER NOT NULL,
       applied_at TEXT NOT NULL,
       package_version TEXT,
       note TEXT
     )"
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS nbadata_ingest_log (
       ingest_id TEXT PRIMARY KEY,
       season INTEGER,
       season_type TEXT,
       started_at TEXT,
       finished_at TEXT,
       n_games INTEGER,
       n_team_box INTEGER,
       n_player_box INTEGER,
       source TEXT,
       ok INTEGER,
       error TEXT
     )"
  )
  invisible(TRUE)
}

.nba_db_create_tables_v1 <- function(con) {
  schema <- .espn_nba_schema()
  .nba_db_create_table_from_schema(con, "games", schema$games, c("game_id"))
  .nba_db_create_table_from_schema(con, "team_box", schema$team_box, c("game_id", "team_id"))
  .nba_db_create_table_from_schema(con, "player_box", schema$player_box, c("game_id", "athlete_id"))
  # TODO: consider player_game_status table for explicit DNP/absence rows.
  invisible(TRUE)
}

.nba_db_create_table_from_schema <- function(con, table, schema, keys) {
  cols <- vapply(names(schema), function(name) {
    type <- switch(
      schema[[name]],
      integer = "INTEGER",
      numeric = "REAL",
      character = "TEXT",
      logical = "INTEGER",
      date = "TEXT",
      posixct = "TEXT",
      "TEXT"
    )
    sprintf("%s %s", name, type)
  }, character(1))

  key_sql <- sprintf("PRIMARY KEY(%s)", paste(keys, collapse = ", "))
  sql <- sprintf(
    "CREATE TABLE IF NOT EXISTS %s (%s, %s)",
    table,
    paste(cols, collapse = ", "),
    key_sql
  )
  DBI::dbExecute(con, sql)
  invisible(TRUE)
}

.nba_db_create_indices <- function(con) {
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_games_season ON games(season, season_type)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_games_date ON games(game_date)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_team_box_game ON team_box(game_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_team_box_team ON team_box(team_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_player_box_game ON player_box(game_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_player_box_athlete ON player_box(athlete_id)")
  invisible(TRUE)
}
