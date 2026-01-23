testthat::test_that("write_parsed_tables writes CSV outputs", {
  testthat::skip_if_not_installed("withr")

  out_dir <- withr::local_tempdir()
  tables <- list(
    games = tibble::tibble(
      game_id = 1L,
      season = 2023L,
      season_type = 2L,
      game_date = as.Date("2023-01-01")
    ),
    team_box = tibble::tibble(
      game_id = 1L,
      team_id = 10L
    ),
    player_box = tibble::tibble(
      game_id = 1L,
      athlete_id = 100L,
      points = 12L,
      home_away = "home"
    ),
    file_index = tibble::tibble(file_path = "summary_2023_20230101_1.json")
  )

  write_parsed_tables(tables, out_dir = out_dir, format = "csv", overwrite = TRUE)

  csv_paths <- file.path(out_dir, c("games.csv", "team_box.csv", "player_box.csv", "file_index.csv"))
  testthat::expect_true(all(file.exists(csv_paths)))

  testthat::expect_equal(nrow(utils::read.csv(csv_paths[[1]])), nrow(tables$games))
  testthat::expect_equal(nrow(utils::read.csv(csv_paths[[2]])), nrow(tables$team_box))
  testthat::expect_equal(nrow(utils::read.csv(csv_paths[[3]])), nrow(tables$player_box))
  testthat::expect_equal(nrow(utils::read.csv(csv_paths[[4]])), nrow(tables$file_index))
})

testthat::test_that("write_parsed_tables writes RDS outputs", {
  testthat::skip_if_not_installed("withr")

  out_dir <- withr::local_tempdir()
  tables <- list(
    games = tibble::tibble(
      game_id = 1L,
      season = 2023L,
      season_type = 2L,
      game_date = as.Date("2023-01-01")
    ),
    team_box = tibble::tibble(
      game_id = 1L,
      team_id = 10L
    ),
    player_box = tibble::tibble(
      game_id = 1L,
      athlete_id = 100L,
      points = 12L,
      home_away = "home"
    ),
    file_index = tibble::tibble(file_path = "summary_2023_20230101_1.json")
  )

  result <- write_parsed_tables(
    tables,
    out_dir = out_dir,
    format = "rds",
    season = 2023,
    overwrite = TRUE
  )

  testthat::expect_true(file.exists(result$rds$path))

  loaded <- readRDS(result$rds$path)
  testthat::expect_true(all(c("games", "team_box", "player_box", "file_index") %in% names(loaded)))
  testthat::expect_equal(nrow(loaded$games), nrow(tables$games))
  testthat::expect_equal(nrow(loaded$team_box), nrow(tables$team_box))
  testthat::expect_equal(nrow(loaded$player_box), nrow(tables$player_box))
  testthat::expect_equal(nrow(loaded$file_index), nrow(tables$file_index))
})

testthat::test_that("write_parsed_tables writes SQLite outputs", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("withr")

  out_dir <- withr::local_tempdir()
  db_path <- file.path(out_dir, "nba.sqlite")
  tables <- list(
    games = tibble::tibble(
      game_id = 1L,
      season = 2023L,
      season_type = 2L,
      game_date = as.Date("2023-01-01")
    ),
    team_box = tibble::tibble(
      game_id = 1L,
      team_id = 10L
    ),
    player_box = tibble::tibble(
      game_id = 1L,
      athlete_id = 100L,
      points = 12L,
      home_away = "home"
    ),
    file_index = tibble::tibble(file_path = "summary_2023_20230101_1.json")
  )

  write_parsed_tables(
    tables,
    out_dir = out_dir,
    format = "sqlite",
    db_path = db_path,
    overwrite = TRUE
  )

  testthat::expect_true(file.exists(db_path))

  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  table_names <- DBI::dbListTables(db)
  testthat::expect_true(all(c("games", "team_box", "player_box", "file_index") %in% table_names))

  count_games <- DBI::dbGetQuery(db, "select count(*) as n from games")
  count_team <- DBI::dbGetQuery(db, "select count(*) as n from team_box")
  count_player <- DBI::dbGetQuery(db, "select count(*) as n from player_box")
  count_file <- DBI::dbGetQuery(db, "select count(*) as n from file_index")

  testthat::expect_equal(count_games$n[[1]], nrow(tables$games))
  testthat::expect_equal(count_team$n[[1]], nrow(tables$team_box))
  testthat::expect_equal(count_player$n[[1]], nrow(tables$player_box))
  testthat::expect_equal(count_file$n[[1]], nrow(tables$file_index))

  schema <- .espn_nba_schema()
  testthat::expect_equal(names(DBI::dbReadTable(db, "games")), names(schema$games))
  testthat::expect_equal(names(DBI::dbReadTable(db, "team_box")), names(schema$team_box))
  testthat::expect_equal(names(DBI::dbReadTable(db, "player_box")), names(schema$player_box))
})
