#' Save parsed tables to an RDS file
#'
#' @param tables Named list from espn_nba_parse_raw_dir()
#' @param season Season year used for the filename
#' @param dir Base directory to store the RDS file
#' @param subdir Optional subdirectory under dir
#' @param overwrite Overwrite existing file when TRUE
#' @return File path to the saved RDS
#' @export
save_parsed_tables_rds <- function(tables,
                                   season,
                                   dir = "data",
                                   subdir = "parsed",
                                   overwrite = FALSE) {
  season <- as.character(season)
  base_dir <- if (is.null(subdir) || !nzchar(subdir)) dir else file.path(dir, subdir)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  path <- file.path(base_dir, sprintf("parsed_%s.rds", season))
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop(sprintf("File already exists: %s", path), call. = FALSE)
  }

  saveRDS(tables, path)
  path
}
