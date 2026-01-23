#' Collect raw JSON, parse, validate, and store for a season
#'
#' @param season Season year
#' @param season_type One of "regular" or "postseason"
#' @param raw_dir Directory to store raw JSON
#' @param parsed_dir Directory to store parsed outputs
#' @param overwrite_raw Overwrite existing raw files
#' @param overwrite_rds Overwrite existing RDS files
#' @param skip_if_exists Skip steps when outputs already exist
#' @param validate Run validate_parsed_tables() when TRUE
#' @param warn_only Emit warnings instead of stopping on validation issues
#' @param ingest_db Write parsed tables to SQLite when TRUE
#' @param db_path SQLite database path
#' @param db_mode One of "replace" or "upsert" for database writes
#' @param con Optional DBI connection
#' @param progress Show a progress bar while collecting
#' @return A list with collect, tables, validation, rds_path, and db_ingest
#' @export
collect_parse_store <- function(season,
                                season_type = c("regular", "postseason"),
                                raw_dir = "data/raw",
                                parsed_dir = file.path("data", "parsed"),
                                overwrite_raw = FALSE,
                                overwrite_rds = FALSE,
                                skip_if_exists = TRUE,
                                validate = TRUE,
                                warn_only = TRUE,
                                ingest_db = FALSE,
                                db_path = "data/sql/nbadata.sqlite",
                                db_mode = NULL,
                                con = NULL,
                                progress = TRUE) {
  if (length(season) > 1) {
    if (isTRUE(ingest_db)) {
      if (is.null(db_mode)) {
        stop("db_mode must be provided when ingest_db = TRUE.", call. = FALSE)
      }
      db_mode <- match.arg(db_mode, c("replace", "upsert"))
    }
    created_con <- FALSE
    db_con <- con
    if (isTRUE(ingest_db) && is.null(db_con)) {
      db_con <- nba_db_connect(db = db_path, drv = "sqlite")
      created_con <- TRUE
    }
    on.exit(if (created_con) nba_db_disconnect(db_con), add = TRUE)

    results <- lapply(season, function(season_value) {
      collect_parse_store(
        season = season_value,
        season_type = season_type,
        raw_dir = raw_dir,
        parsed_dir = parsed_dir,
        overwrite_raw = overwrite_raw,
        overwrite_rds = overwrite_rds,
        skip_if_exists = skip_if_exists,
        validate = validate,
        warn_only = warn_only,
        ingest_db = ingest_db,
        db_path = db_path,
        db_mode = db_mode,
        con = db_con,
        progress = progress
      )
    })
    names(results) <- as.character(season)
    return(results)
  }
  season_type <- match.arg(season_type)
  if (isTRUE(ingest_db)) {
    if (is.null(db_mode)) {
      stop("db_mode must be provided when ingest_db = TRUE.", call. = FALSE)
    }
    db_mode <- match.arg(db_mode, c("replace", "upsert"))
  }

  season_dir <- file.path(raw_dir, season)
  rds_path <- file.path(parsed_dir, sprintf("parsed_%s.rds", season))
  need_collect <- TRUE
  if (isTRUE(skip_if_exists) && !isTRUE(overwrite_raw)) {
    if (dir.exists(season_dir)) {
      existing <- list.files(
        season_dir,
        pattern = sprintf("^summary_%s_.*\\.json$", season),
        full.names = TRUE
      )
      if (length(existing) > 0) {
        need_collect <- FALSE
        message(sprintf("Skipping raw collection for %s: %d files exist.", season, length(existing)))
      }
    }
  }

  collect <- NULL
  if (isTRUE(need_collect)) {
    collect <- collect_raw_season(
      season = season,
      season_type = season_type,
      raw_dir = raw_dir,
      overwrite = overwrite_raw,
      progress = progress
    )
  }

  tables <- NULL
  if (isTRUE(skip_if_exists) && !isTRUE(overwrite_rds) && file.exists(rds_path)) {
    tables <- readRDS(rds_path)
    message(sprintf("Loaded parsed tables from %s", rds_path))
  } else {
    tables <- espn_nba_parse_raw_dir(season, raw_dir = raw_dir, progress = TRUE)
  }
  validation <- NULL
  if (isTRUE(validate)) {
    validation <- validate_parsed_tables(tables, warn_only = warn_only)
  }

  if (!isTRUE(skip_if_exists) || isTRUE(overwrite_rds) || !file.exists(rds_path)) {
    rds_result <- write_parsed_tables(
      tables,
      out_dir = parsed_dir,
      format = "rds",
      rds_path = rds_path,
      season = season,
      overwrite = overwrite_rds
    )
    rds_path <- rds_result$rds$path
    message(sprintf("Saved parsed tables to %s", rds_path))
  }

  db_ingest <- NULL
  if (isTRUE(ingest_db)) {
    created_con <- FALSE
    db_con <- con
    if (is.null(db_con)) {
      db_con <- nba_db_connect(db = db_path, drv = "sqlite")
      created_con <- TRUE
    }
    on.exit(if (created_con) nba_db_disconnect(db_con), add = TRUE)
    db_ingest <- write_parsed_tables(
      tables,
      format = "sqlite",
      mode = db_mode,
      db_path = db_path,
      con = db_con
    )
  }

  list(
    collect = collect,
    tables = tables,
    validation = validation,
    rds_path = rds_path,
    db_ingest = db_ingest
  )
}
