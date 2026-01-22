testthat::test_that("save_parsed_tables_rds writes and overwrites", {
  tmp_dir <- file.path(tempdir(), paste0("nba-parsed-", Sys.getpid()))
  tables <- list(
    games = tibble::tibble(game_id = 1L),
    team_box = tibble::tibble(game_id = 1L, team_id = 10L),
    player_box = tibble::tibble(game_id = 1L, athlete_id = 100L)
  )

  path <- save_parsed_tables_rds(
    tables,
    season = 2023,
    dir = tmp_dir,
    subdir = ""
  )
  testthat::expect_true(file.exists(path))
  reloaded <- readRDS(path)
  testthat::expect_true(is.list(reloaded))
  testthat::expect_true(all(c("games", "team_box", "player_box") %in% names(reloaded)))

  testthat::expect_error(
    save_parsed_tables_rds(
      tables,
      season = 2023,
      dir = tmp_dir,
      subdir = "",
      overwrite = FALSE
    ),
    "already exists"
  )

  path2 <- save_parsed_tables_rds(
    tables,
    season = 2023,
    dir = tmp_dir,
    subdir = "",
    overwrite = TRUE
  )
  testthat::expect_true(file.exists(path2))
})
