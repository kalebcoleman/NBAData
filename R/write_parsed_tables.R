#' Write parsed ESPN NBA tables to CSV and/or SQLite
#'
#' @param tables Named list from espn_nba_parse_raw_dir()
#' @param out_dir Output directory for CSV files and default SQLite path
#' @param format One of "sqlite", "csv", or "both"
#' @param db_path SQLite database path
#' @param overwrite Overwrite existing outputs when TRUE
#' @param create_dirs Create output directories when TRUE
#' @return Invisible list describing written outputs
#' @export
write_parsed_tables <- function(tables,
                                out_dir = "data/parsed",
                                format = c("sqlite", "csv", "both"),
                                db_path = file.path(out_dir, "nba.sqlite"),
                                overwrite = TRUE,
                                create_dirs = TRUE) {
  format <- match.arg(format)
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
    db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    sqlite_tables <- list()
    for (name in names(tables_out)) {
      DBI::dbWriteTable(db, name, tables_out[[name]], overwrite = overwrite)
      sqlite_tables[[name]] <- nrow(tables_out[[name]])
    }
    results$sqlite <- list(db_path = db_path, tables = sqlite_tables)
  }

  invisible(results)
}
