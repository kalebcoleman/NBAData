testthat::test_that("summary key helpers normalize dates", {
  testthat::expect_equal(
    NBAData:::.espn_nba_clean_date_key(as.Date("2023-04-09")),
    "20230409"
  )
  testthat::expect_equal(
    NBAData:::.espn_nba_clean_date_key("2023-04-09T01:30Z"),
    "20230409"
  )
  testthat::expect_equal(
    NBAData:::.espn_nba_summary_key("2023", "20230409", 401),
    "2023_20230409_401"
  )
})

testthat::test_that("failed scrape logging writes CSV rows", {
  tmp_dir <- file.path(tempdir(), paste0("nba_scrape_log_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  path <- NBAData:::log_failed_scrape(
    game_id = 123,
    game_date = "2023-04-09",
    status_code = 500,
    error_message = "Server error",
    dir = tmp_dir,
    filename = "failures.csv"
  )

  testthat::expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE)
  testthat::expect_true(length(lines) >= 2)
  testthat::expect_true(grepl("game_id", lines[1]))
  testthat::expect_true(grepl("123", lines[2]))
})

testthat::test_that("summary_raw_safe returns status metadata", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  result <- espn_nba_summary_raw_safe(401468016, save_raw = FALSE)
  testthat::expect_true(is.list(result))
  testthat::expect_true(all(c("status_code", "raw", "error") %in% names(result)))
})
