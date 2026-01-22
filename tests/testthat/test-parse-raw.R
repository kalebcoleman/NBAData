testthat::test_that("games parser returns expected columns", {
  raw <- list(
    header = list(
      id = "401",
      season = list(year = 2023, type = 2),
      competitions = list(
        list(
          date = "2023-02-02T00:00Z",
          status = list(type = list(state = "post", name = "Final", completed = TRUE)),
          competitors = list(
            list(homeAway = "home", score = "100", winner = TRUE, team = list(id = "1")),
            list(homeAway = "away", score = "90", winner = FALSE, team = list(id = "2"))
          )
        )
      )
    )
  )

  games <- espn_nba_games_from_raw(raw)
  testthat::expect_true(is.data.frame(games))
  testthat::expect_equal(games$game_id, 401L)
  testthat::expect_equal(games$season, 2023L)
  testthat::expect_equal(games$season_type, 2L)
  testthat::expect_equal(games$home_team_id, 1L)
  testthat::expect_equal(games$away_team_id, 2L)
  testthat::expect_equal(games$home_team_score, 100L)
  testthat::expect_equal(games$away_team_score, 90L)
  testthat::expect_true(games$status_completed)
})

testthat::test_that("team box parser returns empty tibble when stats missing", {
  raw <- list(
    header = list(
      id = "401",
      season = list(year = 2023, type = 2),
      competitions = list(
        list(
          date = "2023-02-02T00:00Z",
          status = list(type = list(state = "post", name = "Final", completed = TRUE)),
          competitors = list(
            list(homeAway = "home", score = "100", winner = TRUE, team = list(id = "1")),
            list(homeAway = "away", score = "90", winner = FALSE, team = list(id = "2"))
          )
        )
      )
    ),
    boxscore = list(
      teams = list(
        list(statistics = list()),
        list(statistics = list())
      )
    )
  )

  team_box <- espn_nba_team_box_from_raw(raw)
  testthat::expect_true(is.data.frame(team_box))
  testthat::expect_equal(nrow(team_box), 0)
  testthat::expect_true(all(NBAData:::.espn_nba_schema_names("team_box") %in% colnames(team_box)))
})

testthat::test_that("player box parser returns empty tibble when stats missing", {
  raw <- list(
    header = list(
      id = "401",
      season = list(year = 2023, type = 2),
      competitions = list(
        list(
          date = "2023-02-02T00:00Z",
          status = list(type = list(state = "post", name = "Final", completed = TRUE)),
          competitors = list(
            list(homeAway = "home", score = "100", winner = TRUE, team = list(id = "1")),
            list(homeAway = "away", score = "90", winner = FALSE, team = list(id = "2"))
          )
        )
      )
    ),
    boxscore = list(
      players = list(
        list(statistics = list())
      )
    )
  )

  player_box <- espn_nba_player_box_from_raw(raw)
  testthat::expect_true(is.data.frame(player_box))
  testthat::expect_equal(nrow(player_box), 0)
  testthat::expect_true(all(NBAData:::.espn_nba_schema_names("player_box") %in% colnames(player_box)))
})
