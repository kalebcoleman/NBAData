#' Get current schema version
#'
#' @param con DBI connection
#' @return Integer schema version
#' @export
nba_db_get_version <- function(con) {
  if (!DBI::dbExistsTable(con, "nbadata_meta")) {
    return(0L)
  }
  out <- DBI::dbGetQuery(con, "SELECT MAX(schema_version) AS schema_version FROM nbadata_meta")
  version <- out$schema_version[[1]]
  if (is.na(version)) {
    return(0L)
  }
  as.integer(version)
}

#' Record schema version
#'
#' @param con DBI connection
#' @param version Schema version
#' @param note Optional migration note
#' @return TRUE invisibly
#' @export
nba_db_set_version <- function(con, version, note = NULL) {
  pkg <- tryCatch(utils::packageVersion("NBAData"), error = function(e) NULL)
  pkg <- if (is.null(pkg)) NA_character_ else as.character(pkg)
  DBI::dbExecute(
    con,
    "INSERT INTO nbadata_meta (schema_version, applied_at, package_version, note)
     VALUES (?, ?, ?, ?)",
    params = list(
      as.integer(version),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      pkg,
      note
    )
  )
  invisible(TRUE)
}

#' Apply schema migrations
#'
#' @param con DBI connection
#' @param from Current schema version
#' @param to Target schema version
#' @return TRUE invisibly
#' @export
nba_db_apply_migrations <- function(con, from, to) {
  from <- as.integer(from)
  to <- as.integer(to)
  if (from < 1 && to >= 1) {
    .nba_db_create_tables_v1(con)
    .nba_db_create_indices(con)
    nba_db_set_version(con, 1L, note = "init")
    from <- 1L
  }

  if (from < 2 && to >= 2) {
    .nba_db_migrate_to_v2(con)
    .nba_db_create_indices(con)
    nba_db_set_version(con, 2L, note = "add missing columns")
    from <- 2L
  }

  if (from < 3 && to >= 3) {
    .nba_db_migrate_to_v3(con)
    .nba_db_create_indices(con)
    nba_db_set_version(con, 3L, note = "expand team_box schema")
    from <- 3L
  }

  if (from < 4 && to >= 4) {
    .nba_db_migrate_to_v4(con)
    .nba_db_create_indices(con)
    nba_db_set_version(con, 4L, note = "add lead change metrics to team_box")
    from <- 4L
  }

  if (from < to) {
    stop(sprintf("No migrations defined for target version %s", to), call. = FALSE)
  }

  invisible(TRUE)
}

.nba_db_migrate_to_v2 <- function(con) {
  schema <- .espn_nba_schema()
  .nba_db_add_missing_columns(con, "games", schema$games)
  .nba_db_add_missing_columns(con, "team_box", schema$team_box)
  .nba_db_add_missing_columns(con, "player_box", schema$player_box)
  invisible(TRUE)
}

.nba_db_migrate_to_v3 <- function(con) {
  schema <- .espn_nba_schema()
  .nba_db_add_missing_columns(con, "team_box", schema$team_box)
  invisible(TRUE)
}

.nba_db_migrate_to_v4 <- function(con) {
  schema <- .espn_nba_schema()
  .nba_db_add_missing_columns(con, "team_box", schema$team_box)
  invisible(TRUE)
}

.nba_db_add_missing_columns <- function(con, table, schema) {
  info <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", table))
  existing <- info$name
  desired <- names(schema)
  missing <- setdiff(desired, existing)
  if (length(missing) == 0) {
    return(invisible(TRUE))
  }

  for (col in missing) {
    type <- switch(
      schema[[col]],
      integer = "INTEGER",
      numeric = "REAL",
      character = "TEXT",
      logical = "INTEGER",
      date = "TEXT",
      posixct = "TEXT",
      "TEXT"
    )
    table_sql <- as.character(DBI::dbQuoteIdentifier(con, table))
    col_sql <- as.character(DBI::dbQuoteIdentifier(con, col))
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table_sql, col_sql, type))
  }

  invisible(TRUE)
}
