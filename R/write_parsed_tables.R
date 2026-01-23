#' Write parsed ESPN NBA tables to CSV and/or SQLite
#'
#' @param tables Named list from espn_nba_parse_raw_dir()
#' @param out_dir Output directory for CSV files and default SQLite path
#' @param format One of "sqlite", "csv", or "both"
#' @param db_path SQLite database path
#' @param con Optional DBI connection
#' @param mode One of "replace" or "upsert" for database writes
#' @param overwrite Overwrite existing outputs when TRUE
#' @param create_dirs Create output directories when TRUE
#' @return Invisible list describing written outputs
#' @export
write_parsed_tables <- function(tables,
                                out_dir = "data/parsed",
                                format = c("sqlite", "csv", "both"),
                                db_path = file.path(out_dir, "nba.sqlite"),
                                con = NULL,
                                mode = c("replace", "upsert"),
                                overwrite = TRUE,
                                create_dirs = TRUE) {
  format <- match.arg(format)
  mode <- match.arg(mode)
  formats <- if (format == "both") c("sqlite", "csv") else format

  required <- c("games", "team_box", "player_box")
  missing <- setdiff(required, names(tables))
  if (length(missing) > 0) {
    stop(sprintf("Missing required tables: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }

  tables_out <- lapply(required, function(name) {
    .espn_nba_apply_schema(tables[[name]], name)
  })
  names(tables_out) <- required
  if (!is.null(tables$file_index)) {
    tables_out$file_index <- .espn_nba_apply_schema(tables$file_index, "file_index")
  }

  if (isTRUE(create_dirs)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  }

  results <- list(format = formats)

  if ("csv" %in% formats) {
    csv_files <- list()
    for (name in names(tables_out)) {
      path <- file.path(out_dir, sprintf("%s.csv", name))
      if (file.exists(path) && !isTRUE(overwrite)) {
        stop(sprintf("CSV already exists: %s", path), call. = FALSE)
      }
      utils::write.csv(tables_out[[name]], path, row.names = FALSE)
      csv_files[[name]] <- list(path = path, n = nrow(tables_out[[name]]))
    }
    results$csv <- list(out_dir = out_dir, files = csv_files)
  }

  if ("sqlite" %in% formats) {
    created_con <- FALSE
    db <- con
    if (is.null(db)) {
      db <- nba_db_connect(db = db_path, drv = "sqlite")
      created_con <- TRUE
    }
    on.exit(if (created_con) nba_db_disconnect(db), add = TRUE)
    nba_db_init(db, target_version = NBA_SCHEMA_VERSION)

    sqlite_tables <- list()
    if (mode == "replace") {
      for (name in names(tables_out)) {
        DBI::dbWriteTable(db, name, tables_out[[name]], overwrite = overwrite)
        sqlite_tables[[name]] <- nrow(tables_out[[name]])
      }
    } else {
      games_before <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM games")$n[[1]]
      .nba_db_upsert_table(db, "games", tables_out$games, c("game_id"))
      games_after <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM games")$n[[1]]
      if (nrow(tables_out$games) > 0 && games_after == games_before) {
        warning("Upsert for games did not change row count; verify keys or data availability.", call. = FALSE)
      }

      team_box_before <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM team_box")$n[[1]]
      .nba_db_upsert_table(db, "team_box", tables_out$team_box, c("game_id", "team_id"))
      team_box_after <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM team_box")$n[[1]]
      if (nrow(tables_out$team_box) > 0 && team_box_after == team_box_before) {
        warning("Upsert for team_box did not change row count; verify keys or data availability.", call. = FALSE)
      }

      player_box_before <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM player_box")$n[[1]]
      .nba_db_upsert_table(db, "player_box", tables_out$player_box, c("game_id", "athlete_id"))
      player_box_after <- DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM player_box")$n[[1]]
      if (nrow(tables_out$player_box) > 0 && player_box_after == player_box_before) {
        warning("Upsert for player_box did not change row count; verify keys or data availability.", call. = FALSE)
      }
      sqlite_tables$games <- nrow(tables_out$games)
      sqlite_tables$team_box <- nrow(tables_out$team_box)
      sqlite_tables$player_box <- nrow(tables_out$player_box)
    }
    results$sqlite <- list(db_path = db_path, tables = sqlite_tables, mode = mode)
  }

  invisible(results)
}

.nba_db_upsert_table <- function(con, table, df, key_cols, chunk_size = 1000) {
  if (is.null(df) || nrow(df) == 0) {
    return(invisible(0L))
  }

  db_cols <- DBI::dbListFields(con, table)
  missing_keys <- setdiff(key_cols, db_cols)
  if (length(missing_keys) > 0) {
    stop(
      sprintf("Table %s is missing key columns (%s).", table, paste(missing_keys, collapse = ", ")),
      call. = FALSE
    )
  }
  extra <- setdiff(names(df), db_cols)
  if (length(extra) > 0) {
    stop(
      sprintf(
        "Table %s has extra columns (%s). Bump NBA_SCHEMA_VERSION and add a migration.",
        table,
        paste(extra, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  missing <- setdiff(db_cols, names(df))
  if (length(missing) > 0) {
    for (col in missing) {
      df[[col]] <- NA
    }
  }
  df <- df[, db_cols, drop = FALSE]
  if (length(setdiff(key_cols, names(df))) > 0) {
    stop(sprintf("Missing key columns in %s data frame.", table), call. = FALSE)
  }
  if (any(!stats::complete.cases(df[, key_cols, drop = FALSE]))) {
    stop(sprintf("Key columns for %s contain NA values.", table), call. = FALSE)
  }

  is_date <- vapply(df, inherits, logical(1), "Date")
  if (any(is_date)) {
    for (col in names(df)[is_date]) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  is_posix <- vapply(df, inherits, logical(1), "POSIXt")
  if (any(is_posix)) {
    for (col in names(df)[is_posix]) {
      df[[col]] <- format(df[[col]], "%Y-%m-%d %H:%M:%S")
    }
  }

  cols <- names(df)
  col_sql <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")
  non_keys <- setdiff(cols, key_cols)

  total_rows <- 0L
  n <- nrow(df)
  idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  DBI::dbWithTransaction(con, {
    stg_table <- paste0("stg_", table, "_", as.integer(stats::runif(1, 1, 1e9)))
    stg_id <- DBI::dbQuoteIdentifier(con, stg_table)
    table_id <- DBI::dbQuoteIdentifier(con, table)
    on.exit(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", stg_id)), add = TRUE)
    DBI::dbExecute(
      con,
      paste("CREATE TABLE", stg_id, "AS SELECT", col_sql, "FROM", table_id, "WHERE 0")
    )
    for (rows in idx) {
      chunk <- df[rows, , drop = FALSE]
      DBI::dbWriteTable(con, stg_table, chunk, append = TRUE)
      total_rows <- total_rows + nrow(chunk)
    }
    stg_count <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", stg_id))$n[[1]]
    if (isTRUE(getOption("nbadata.verbose"))) {
      message(sprintf("Staging %s rows in %s", stg_count, stg_table))
    }
    if (stg_count == 0 && n > 0) {
      stop(sprintf("Staging table %s is empty after insert.", stg_table), call. = FALSE)
    }
    key_ids <- DBI::dbQuoteIdentifier(con, key_cols)
    key_match <- paste(sprintf("t.%s = s.%s", key_ids, key_ids), collapse = " AND ")
    delete_sql <- paste0(
      "DELETE FROM ", table_id, " AS t WHERE EXISTS (",
      "SELECT 1 FROM ", stg_id, " AS s WHERE ", key_match, ")"
    )
    DBI::dbExecute(con, delete_sql)
    insert_sql <- paste0(
      "INSERT INTO ", table_id, " (", col_sql, ") ",
      "SELECT ", col_sql, " FROM ", stg_id
    )
    DBI::dbExecute(con, insert_sql)
  })
  options_verbose <- isTRUE(getOption("nbadata.verbose"))
  if (options_verbose) {
    message(sprintf("Upserted %s rows into %s", total_rows, table))
  }
  invisible(total_rows)
}
