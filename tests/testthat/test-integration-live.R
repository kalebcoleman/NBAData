testthat::test_that("live integration fetches and parses a game", {
  testthat::skip_on_cran()
  testthat::skip_if_not(Sys.getenv("NBDATA_LIVE") == "true")

  today <- format(Sys.Date(), "%Y%m%d")
  games <- espn_nba_scoreboard(today)
  if (is.null(games) || nrow(games) == 0) {
    testthat::skip("No games found for today.")
  }

  game_id <- games$game_id[[1]]
  raw <- espn_nba_summary_raw(game_id)
  games_tbl <- espn_nba_games_from_raw(raw)
  team_box <- espn_nba_team_box_from_raw(raw)
  player_box <- espn_nba_player_box_from_raw(raw)

  testthat::expect_true(nrow(games_tbl) >= 1)
  testthat::expect_true(is.data.frame(team_box))
  testthat::expect_true(is.data.frame(player_box))
})
