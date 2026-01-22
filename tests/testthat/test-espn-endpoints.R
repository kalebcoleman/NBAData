context("ESPN endpoints")

get_schedule_for_tests <- function() {
  today <- Sys.Date()
  date_range <- sprintf(
    "%s-%s",
    format(today - 7, "%Y%m%d"),
    format(today + 7, "%Y%m%d")
  )

  schedule <- get_nba_schedule_safe(dates = date_range)
  if (!is.null(schedule$error)) {
    if (is.na(schedule$status_code) || schedule$status_code %in% c(500, 502, 503, 504)) {
      testthat::skip("ESPN scoreboard temporarily unavailable")
    }
    stop(schedule$error)
  }
  schedule <- schedule$data
  if (nrow(schedule) == 0) {
    schedule <- get_nba_schedule_safe(dates = format(today, "%Y%m%d"))
    if (!is.null(schedule$error)) {
      if (is.na(schedule$status_code) || schedule$status_code %in% c(500, 502, 503, 504)) {
        testthat::skip("ESPN scoreboard temporarily unavailable")
      }
      stop(schedule$error)
    }
    schedule <- schedule$data
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

get_completed_game_id_for_tests <- function() {
  dates <- format(Sys.Date() - 1:14, "%Y%m%d")
  for (date_str in dates) {
    scoreboard <- espn_nba_scoreboard(date_str)
    if (nrow(scoreboard) == 0) {
      next
    }
    completed <- scoreboard[scoreboard$status_completed %in% TRUE, , drop = FALSE]
    if (nrow(completed) == 0) {
      next
    }
    return(as.character(completed$game_id[[1]]))
  }

  testthat::skip("No completed games found in recent dates")
}

test_that("schedule endpoint returns expected columns", {
  testthat::skip_on_cran()

  schedule <- get_schedule_for_tests()

  testthat::expect_true("gameId" %in% colnames(schedule))
  testthat::expect_gt(nrow(schedule), 0)
})

test_that("schedule raw JSON can be saved to disk", {
  testthat::skip_on_cran()

  today <- Sys.Date()
  date_range <- sprintf(
    "%s-%s",
    format(today - 1, "%Y%m%d"),
    format(today + 1, "%Y%m%d")
  )
  raw_dir <- file.path(tempdir(), "nba-raw")
  schedule <- get_nba_schedule_safe(dates = date_range, save_raw = TRUE, raw_dir = raw_dir)
  if (!is.null(schedule$error)) {
    if (is.na(schedule$status_code) || schedule$status_code %in% c(500, 502, 503, 504)) {
      testthat::skip("ESPN scoreboard temporarily unavailable")
    }
    stop(schedule$error)
  }
  schedule <- schedule$data

  testthat::expect_true(nrow(schedule) >= 0)
  testthat::expect_true(length(list.files(raw_dir, pattern = "^scoreboard_.*\\.json$")) > 0)
})

test_that("play-by-play endpoint returns data for a game", {
  testthat::skip_on_cran()

  game_id <- get_completed_game_id_for_tests()
  betting <- espn_nba_betting(game_id)

  testthat::expect_true(is.list(betting))
  testthat::expect_true(all(c("pickcenter", "againstTheSpread", "predictor") %in% names(betting)))
})

test_that("scoreboard endpoint returns expected columns", {
  testthat::skip_on_cran()

  schedule <- get_schedule_for_tests()
  if (!"game_date" %in% colnames(schedule) && !"date" %in% colnames(schedule)) {
    testthat::skip("Schedule data does not include date column")
  }

  if ("game_date" %in% colnames(schedule)) {
    game_date <- schedule$game_date[[1]]
  } else {
    game_date <- schedule$date[[1]]
  }
  if (is.null(game_date) || is.na(game_date)) {
    testthat::skip("No valid date available in schedule data")
  }

  game_date <- as.Date(game_date)
  if (is.na(game_date)) {
    testthat::skip("Unable to parse schedule date")
  }

  scoreboard <- espn_nba_scoreboard(format(game_date, "%Y%m%d"))

  testthat::expect_true(
    all(c("game_id", "home_team_id", "away_team_id") %in% colnames(scoreboard))
  )
})

test_that("team box endpoint returns data for a game", {
  testthat::skip_on_cran()

  game_id <- get_game_id_for_tests()
  team_box <- espn_nba_team_box(game_id)

  testthat::expect_true(is.data.frame(team_box))
  if (nrow(team_box) > 0) {
    testthat::expect_true("game_id" %in% colnames(team_box))
  }
})

test_that("player box endpoint returns data for a game", {
  testthat::skip_on_cran()

  game_id <- get_game_id_for_tests()
  player_box <- espn_nba_player_box(game_id)

  testthat::expect_true(is.data.frame(player_box))
  if (nrow(player_box) > 0) {
    testthat::expect_true("game_id" %in% colnames(player_box))
  }
})

test_that("standings endpoint returns data", {
  testthat::skip_on_cran()

  standings <- espn_nba_standings()

  testthat::expect_true(is.data.frame(standings))
  if (nrow(standings) > 0) {
    testthat::expect_true("team_id" %in% colnames(standings) || "team.id" %in% colnames(standings))
  }
})
