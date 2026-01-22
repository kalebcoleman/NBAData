# Extracted from test-parse-raw-dir.R:27

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "NBAData", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
raw_dir <- file.path(tempdir(), paste0("nba-raw-", Sys.getpid()))
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
    list(header = list(
      id = "1",
      season = list(year = 2023, type = 2),
      competitions = list(
        list(
          date = "2023-04-09T00:00Z",
          status = list(type = list(state = "post", name = "Final", completed = TRUE)),
          competitors = list(
            list(homeAway = "home", score = "100", winner = TRUE, team = list(id = "1")),
            list(homeAway = "away", score = "90", winner = FALSE, team = list(id = "2"))
          )
        )
      )
    )),
    file.path(raw_dir, "summary_2023_20230409_1.json")
  )
jsonlite::write_json(
    list(header = list(id = "2", season = list(year = 2024, type = 2))),
    file.path(raw_dir, "summary_2024_20240101_2.json")
  )
out <- espn_nba_parse_raw_dir(2023, raw_dir = raw_dir)
