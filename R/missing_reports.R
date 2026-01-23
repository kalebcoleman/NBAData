#' Join missing boxscore report with parsed game statuses
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param raw_dir Directory containing raw JSON and missing CSVs
#' @param tables Optional parsed tables list from espn_nba_parse_raw_dir()
#' @param tables_path Optional path to a parsed RDS created by write_parsed_tables()
#' @return A tibble with missing boxscore rows and status fields
#' @export
espn_nba_missing_boxscore_status <- function(season,
                                             season_type = c("regular", "postseason", "all"),
                                             raw_dir = "data/raw",
                                             tables = NULL,
                                             tables_path = NULL) {
  season_type <- match.arg(season_type)
  season_dir <- file.path(raw_dir, season)
  source_dir <- if (dir.exists(season_dir)) season_dir else raw_dir
  missing_path <- file.path(source_dir, sprintf("missing_boxscore_%s_%s.csv", season, season_type))

  if (!file.exists(missing_path)) {
    return(tibble::tibble())
  }

  missing <- utils::read.csv(missing_path)
  if (is.null(tables)) {
    if (!is.null(tables_path)) {
      tables <- readRDS(tables_path)
    } else {
      default_path <- file.path("data", "parsed", sprintf("parsed_%s.rds", season))
      if (file.exists(default_path)) {
        tables <- readRDS(default_path)
      } else {
        tables <- espn_nba_parse_raw_dir(season, raw_dir = raw_dir, progress = FALSE)
      }
    }
  }

  dplyr::left_join(
    missing,
    dplyr::select(
      tables$games,
      game_id,
      game_date,
      status_name,
      status_state,
      status_completed
    ),
    by = "game_id"
  )
}
