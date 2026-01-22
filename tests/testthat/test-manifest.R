make_summary_fixture <- function(with_boxscore = FALSE, with_betting = FALSE) {
  raw <- list()
  if (isTRUE(with_boxscore)) {
    raw$boxscore <- list(
      teams = list(
        list(statistics = list(list(name = "fg", displayValue = "1"))),
        list(statistics = list(list(name = "fg", displayValue = "1")))
      ),
      players = list(
        list(statistics = list(list(name = "min", displayValue = "1")))
      )
    )
  }
  if (isTRUE(with_betting)) {
    raw$pickcenter <- list(list())
  }
  raw
}

testthat::test_that("scraped game ids are parsed from filenames", {
  raw_dir <- file.path(tempdir(), paste0("nba-raw-", Sys.getpid()))
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  jsonlite::write_json(make_summary_fixture(), file.path(raw_dir, "summary_2023_20230409_401.json"))
  jsonlite::write_json(make_summary_fixture(), file.path(raw_dir, "summary_2023_20230410_402.json"))

  ids <- espn_nba_scraped_game_ids(raw_dir = raw_dir)
  testthat::expect_true(all(c(401, 402) %in% ids))
})

testthat::test_that("manifest reports scraped status and flags", {
  raw_dir <- file.path(tempdir(), paste0("nba-raw-", Sys.getpid()))
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  jsonlite::write_json(
    make_summary_fixture(with_boxscore = TRUE, with_betting = TRUE),
    file.path(raw_dir, "summary_2023_20230409_401.json")
  )
  jsonlite::write_json(
    make_summary_fixture(with_boxscore = FALSE, with_betting = FALSE),
    file.path(raw_dir, "summary_2023_20230410_402.json")
  )

  testthat::local_mocked_bindings(
    espn_nba_expected_game_ids = function(...) c(401L, 402L, 403L),
    .env = asNamespace("NBAData")
  )

  manifest <- espn_nba_manifest(
    season = 2023,
    season_type = "regular",
    raw_dir = raw_dir,
    check_boxscore = TRUE,
    check_betting = TRUE,
    write_csv = FALSE
  )

  testthat::expect_equal(nrow(manifest), 3)
  testthat::expect_true(all(c(401L, 402L, 403L) %in% manifest$game_id))
  testthat::expect_true(manifest$scraped[manifest$game_id == 401L])
  testthat::expect_true(manifest$has_boxscore[manifest$game_id == 401L])
  testthat::expect_true(manifest$has_betting[manifest$game_id == 401L])
  testthat::expect_false(manifest$scraped[manifest$game_id == 403L])
})

testthat::test_that("validate_season enforces completeness thresholds", {
  raw_dir <- file.path(tempdir(), paste0("nba-raw-", Sys.getpid()))
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  jsonlite::write_json(
    make_summary_fixture(with_boxscore = TRUE, with_betting = FALSE),
    file.path(raw_dir, "summary_2023_20230409_401.json")
  )

  testthat::local_mocked_bindings(
    espn_nba_expected_game_ids = function(...) c(401L, 402L, 403L),
    .env = asNamespace("NBAData")
  )

  testthat::expect_error(
    validate_season(
      season = 2023,
      season_type = "regular",
      raw_dir = raw_dir,
      min_completeness = 0.9
    ),
    "completeness"
  )

  manifest <- validate_season(
    season = 2023,
    season_type = "regular",
    raw_dir = raw_dir,
    min_completeness = 0.3
  )
  testthat::expect_true(inherits(manifest, "data.frame"))
})
