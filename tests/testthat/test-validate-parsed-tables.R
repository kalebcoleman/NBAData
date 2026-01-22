testthat::test_that("validate_parsed_tables passes happy path", {
  games <- tibble::tibble(
    game_id = c(1L, 2L),
    game_date = as.Date(c("2023-01-01", "2023-01-02")),
    status_completed = c(TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L, 2L),
    team_id = c(10L, 11L, 12L, 13L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L, 2L),
    athlete_id = c(100L, 200L, 201L)
  )

  out <- validate_parsed_tables(
    tables = list(games = games, team_box = team_box, player_box = player_box),
    warn_only = TRUE
  )
  testthat::expect_true(out$ok)
  testthat::expect_equal(nrow(out$issues), 0)
})

testthat::test_that("validate_parsed_tables flags team_box row count issues", {
  games <- tibble::tibble(
    game_id = c(1L, 2L),
    game_date = as.Date(c("2023-01-01", "2023-01-02")),
    status_completed = c(TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L),
    team_id = c(10L, 11L, 12L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L),
    athlete_id = c(100L, 200L)
  )

  testthat::expect_error(
    validate_parsed_tables(list(games = games, team_box = team_box, player_box = player_box)),
    "team_box_row_count"
  )
})

testthat::test_that("validate_parsed_tables flags duplicate player keys", {
  games <- tibble::tibble(
    game_id = 1L,
    game_date = as.Date("2023-01-01"),
    status_completed = TRUE
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L),
    team_id = c(10L, 11L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 1L),
    athlete_id = c(100L, 100L)
  )

  testthat::expect_error(
    validate_parsed_tables(list(games = games, team_box = team_box, player_box = player_box)),
    "player_box_duplicate"
  )
})

testthat::test_that("validate_parsed_tables returns missingness report", {
  games <- tibble::tibble(
    game_id = c(1L, 2L),
    game_date = as.Date(c("2023-01-01", "2023-01-02")),
    status_completed = c(TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L, 2L),
    team_id = c(10L, NA_integer_, 12L, 13L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L),
    athlete_id = c(100L, 200L)
  )

  out <- validate_parsed_tables(
    tables = list(games = games, team_box = team_box, player_box = player_box),
    warn_only = TRUE
  )
  report <- out$missingness$team_box
  testthat::expect_true(all(c("column", "n_missing", "pct_missing") %in% names(report)))
  testthat::expect_true(any(report$column == "team_id"))
  testthat::expect_true(report$pct_missing[report$column == "team_id"] > 0)
})

testthat::test_that("validate_parsed_tables returns sample buckets", {
  games <- tibble::tibble(
    game_id = c(1L, 2L, 3L),
    game_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    status_completed = c(TRUE, TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    team_id = c(10L, 11L, 12L, 13L, 14L, 15L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L, 3L),
    athlete_id = c(100L, 200L, 300L)
  )

  out <- validate_parsed_tables(
    tables = list(games = games, team_box = team_box, player_box = player_box),
    warn_only = TRUE
  )
  testthat::expect_true(all(c("early", "mid", "late") %in% names(out$samples)))
  testthat::expect_true(length(out$samples$early) > 0)
  testthat::expect_true(length(out$samples$mid) > 0)
  testthat::expect_true(length(out$samples$late) > 0)
})
