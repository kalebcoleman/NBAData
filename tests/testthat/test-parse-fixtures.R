testthat::test_that("fixtures parse into expected tables", {
  raw <- read_fixture_json("completed_with_boxscore.json")
  games <- espn_nba_games_from_raw(raw)
  team_box <- espn_nba_team_box_from_raw(raw)
  player_box <- espn_nba_player_box_from_raw(raw)

  testthat::expect_true(is.data.frame(games))
  testthat::expect_true(all(NBAData:::.espn_nba_schema_names("games") %in% names(games)))
  testthat::expect_true(all(NBAData:::.espn_nba_schema_names("team_box") %in% names(team_box)))
  testthat::expect_true(all(NBAData:::.espn_nba_schema_names("player_box") %in% names(player_box)))
})

testthat::test_that("postponed fixture sets status_completed FALSE", {
  raw <- read_fixture_json("postponed_or_canceled.json")
  games <- espn_nba_games_from_raw(raw)

  testthat::expect_equal(games$status_name[[1]], "STATUS_POSTPONED")
  testthat::expect_false(games$status_completed[[1]])
})

testthat::test_that("validator ignores postponed games", {
  raw <- read_fixture_json("postponed_or_canceled.json")
  games <- espn_nba_games_from_raw(raw)
  team_box <- espn_nba_team_box_from_raw(raw)
  player_box <- espn_nba_player_box_from_raw(raw)

  out <- validate_parsed_tables(
    tables = list(games = games, team_box = team_box, player_box = player_box),
    warn_only = TRUE
  )
  testthat::expect_true(out$ok)
})
