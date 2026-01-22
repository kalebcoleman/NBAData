# Extracted from test-espn-endpoints.R:83

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "NBAData", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("ESPN endpoints")
get_schedule_for_tests <- function() {
  season <- suppressWarnings(as.integer(Sys.getenv("NBA_DATA_TEST_SEASON", "")))
  if (is.na(season)) {
    season <- as.integer(format(Sys.Date(), "%Y"))
  }

  schedule <- get_nba_schedule(season)
  if (nrow(schedule) == 0) {
    schedule <- get_nba_schedule(season - 1)
  }

  if (nrow(schedule) == 0) {
    testthat::skip("No schedule data available for test seasons")
  }

  schedule
}
get_game_id_for_tests <- function() {
  schedule <- get_schedule_for_tests()

  if (!"gameId" %in% colnames(schedule)) {
    testthat::skip("Schedule data does not include gameId column")
  }

  game_id <- schedule$gameId[[1]]
  if (is.null(game_id) || is.na(game_id) || !nzchar(as.character(game_id))) {
    testthat::skip("No valid gameId available in schedule data")
  }

  as.character(game_id)
}

# test -------------------------------------------------------------------------
testthat::skip_on_cran()
game_id <- get_game_id_for_tests()
