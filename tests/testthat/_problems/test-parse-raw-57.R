# Extracted from test-parse-raw.R:57

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "NBAData", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
