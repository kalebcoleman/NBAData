#' Get expected game ids for a season
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @param require_completed TRUE to only return completed games
#' @return Integer vector of expected game ids
#' @export
espn_nba_expected_game_ids <- function(season,
                                       season_type = c("regular", "postseason", "all"),
                                       start_date = NULL,
                                       end_date = NULL,
                                       require_completed = TRUE) {
  season_type <- match.arg(season_type)

  if (isTRUE(require_completed)) {
    games <- espn_nba_season_game_ids(
      season = season,
      season_type = season_type,
      start_date = start_date,
      end_date = end_date
    )
  } else {
    dates <- .espn_nba_season_date_range(season, season_type, start_date, end_date)
    if (length(dates) == 0) {
      return(integer())
    }

    games <- lapply(dates, function(x) {
      espn_nba_scoreboard(format(x, "%Y%m%d"))
    })
    games <- dplyr::bind_rows(games)

    if (nrow(games) == 0) {
      return(integer())
    }

    if (season_type == "regular") {
      games <- games[games$season_type == 2, , drop = FALSE]
    } else if (season_type == "postseason") {
      games <- games[games$season_type == 3, , drop = FALSE]
    }
  }

  unique(as.integer(games$game_id))
}

.espn_nba_parse_game_id_from_filename <- function(paths) {
  vapply(paths, function(path) {
    name <- basename(path)
    name <- sub("\\.json$", "", name)
    parts <- strsplit(name, "_", fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      return(NA_integer_)
    }
    suppressWarnings(as.integer(parts[[length(parts)]]))
  }, integer(1), USE.NAMES = FALSE)
}

#' Get game ids from scraped summary JSON files
#'
#' @param raw_dir Directory to store raw JSON
#' @return Integer vector of scraped game ids
#' @export
espn_nba_scraped_game_ids <- function(raw_dir = "data/raw") {
  files <- list.files(raw_dir, pattern = "^summary_.*\\.json$", full.names = TRUE, recursive = TRUE)
  ids <- .espn_nba_parse_game_id_from_filename(files)
  unique(stats::na.omit(ids))
}

#' Build a manifest for a season's raw summary files
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @param raw_dir Directory to store raw JSON
#' @param check_boxscore TRUE to check if boxscore data exists
#' @param check_betting TRUE to check if betting data exists
#' @param write_csv TRUE to write manifest CSV to disk
#' @param write_missing_boxscore TRUE to write missing boxscore CSV to disk
#' @param write_missing_betting TRUE to write missing betting CSV to disk
#' @return A tibble manifest of expected and scraped games
#' @export
espn_nba_manifest <- function(season,
                              season_type = c("regular", "postseason", "all"),
                              start_date = NULL,
                              end_date = NULL,
                              raw_dir = "data/raw",
                              check_boxscore = TRUE,
                              check_betting = FALSE,
                              write_csv = TRUE,
                              write_missing_boxscore = TRUE,
                              write_missing_betting = FALSE) {
  season_type <- match.arg(season_type)
  season_dir <- file.path(raw_dir, season)
  source_dir <- if (dir.exists(season_dir)) season_dir else raw_dir
  expected_ids <- espn_nba_expected_game_ids(
    season = season,
    season_type = season_type,
    start_date = start_date,
    end_date = end_date,
    require_completed = TRUE
  )

  files <- list.files(source_dir, pattern = "^summary_.*\\.json$", full.names = TRUE)
  file_ids <- .espn_nba_parse_game_id_from_filename(files)
  file_info <- tibble::tibble(
    game_id = file_ids,
    file_path = files
  )
  file_info <- dplyr::filter(file_info, !is.na(game_id))
  file_info <- dplyr::distinct(file_info, game_id, .keep_all = TRUE)

  manifest <- tibble::tibble(game_id = as.integer(expected_ids))
  if (nrow(manifest) == 0) {
    manifest <- dplyr::mutate(
      manifest,
      expected = logical(),
      scraped = logical(),
      file_path = character(),
      has_boxscore = logical(),
      has_betting = logical()
    )
    return(manifest)
  }

  manifest <- dplyr::mutate(manifest, expected = TRUE)
  manifest <- dplyr::left_join(manifest, file_info, by = "game_id")
  manifest <- dplyr::mutate(manifest, scraped = !is.na(file_path))

  has_boxscore <- rep(NA, nrow(manifest))
  has_betting <- rep(NA, nrow(manifest))
  if (isTRUE(check_boxscore) || isTRUE(check_betting)) {
    results <- lapply(seq_len(nrow(manifest)), function(i) {
      path <- manifest$file_path[[i]]
      if (is.na(path)) {
        return(list(boxscore = NA, betting = NA))
      }
      raw <- tryCatch(
        jsonlite::fromJSON(path, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (is.null(raw)) {
        return(list(boxscore = NA, betting = NA))
      }
      list(
        boxscore = if (isTRUE(check_boxscore)) espn_nba_summary_has_boxscore(raw) else NA,
        betting = if (isTRUE(check_betting)) espn_nba_summary_has_betting(raw) else NA
      )
    })
    has_boxscore <- vapply(results, function(x) x$boxscore, logical(1))
    has_betting <- vapply(results, function(x) x$betting, logical(1))
  }

  manifest$has_boxscore <- has_boxscore
  manifest$has_betting <- has_betting

  if (isTRUE(write_csv)) {
    path <- file.path(source_dir, sprintf("manifest_%s_%s.csv", season, season_type))
    utils::write.csv(manifest, path, row.names = FALSE)
  }

  if (isTRUE(write_missing_boxscore) && "has_boxscore" %in% names(manifest)) {
    missing_box <- dplyr::filter(
      manifest,
      scraped %in% TRUE,
      is.na(has_boxscore) | has_boxscore %in% FALSE
    )
    if (nrow(missing_box) > 0) {
      missing_box$status_name <- vapply(missing_box$file_path, function(path) {
        raw <- tryCatch(
          jsonlite::fromJSON(path, simplifyVector = FALSE),
          error = function(e) NULL
        )
        if (is.null(raw)) {
          return(NA_character_)
        }
        status_type <- raw$header$competitions$status$type
        if (is.null(status_type)) {
          return(NA_character_)
        }
        if (is.list(status_type) && length(status_type) > 0) {
          status_type <- status_type[[1]]
        }
        name <- status_type$name
        if (is.null(name) || length(name) == 0) {
          return(NA_character_)
        }
        if (is.list(name)) {
          name <- name[[1]]
        }
        if (length(name) == 0 || is.null(name)) {
          return(NA_character_)
        }
        as.character(name[[1]])
      }, character(1))
      missing_box <- dplyr::filter(
        missing_box,
        !(status_name %in% c("STATUS_POSTPONED", "STATUS_CANCELED"))
      )
      missing_box$status_name <- NULL
    }
    path <- file.path(source_dir, sprintf("missing_boxscore_%s_%s.csv", season, season_type))
    utils::write.csv(missing_box, path, row.names = FALSE)
  }

  if (isTRUE(check_betting) && isTRUE(write_missing_betting) && "has_betting" %in% names(manifest)) {
    missing_betting <- dplyr::filter(
      manifest,
      scraped %in% TRUE,
      is.na(has_betting) | has_betting %in% FALSE
    )
    path <- file.path(source_dir, sprintf("missing_betting_%s_%s.csv", season, season_type))
    utils::write.csv(missing_betting, path, row.names = FALSE)
  }

  manifest
}

#' Validate a season's raw summary completeness
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @param raw_dir Directory to store raw JSON
#' @param min_completeness Minimum share of expected games that must be scraped
#' @param require_boxscore TRUE to require boxscore availability for scraped games
#' @param require_betting TRUE to require betting availability for scraped games
#' @return Manifest invisibly when validation passes
#' @export
validate_season <- function(season,
                            season_type = c("regular", "postseason", "all"),
                            start_date = NULL,
                            end_date = NULL,
                            raw_dir = "data/raw",
                            min_completeness = 0.99,
                            require_boxscore = FALSE,
                            require_betting = FALSE) {
  season_type <- match.arg(season_type)
  manifest <- espn_nba_manifest(
    season = season,
    season_type = season_type,
    start_date = start_date,
    end_date = end_date,
    raw_dir = raw_dir,
    check_boxscore = require_boxscore,
    check_betting = require_betting,
    write_csv = FALSE
  )

  expected_total <- nrow(manifest)
  if (expected_total == 0) {
    stop("No expected games found for the requested season.", call. = FALSE)
  }

  scraped_expected <- sum(manifest$scraped, na.rm = TRUE)
  completeness <- scraped_expected / expected_total

  if (completeness < min_completeness) {
    missing_ids <- manifest$game_id[!manifest$scraped]
    sample_ids <- utils::head(missing_ids, 10)
    stop(
      sprintf(
        "Season completeness %.2f below threshold %.2f. Missing %d games (sample: %s).",
        completeness,
        min_completeness,
        length(missing_ids),
        paste(sample_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (isTRUE(require_boxscore)) {
    missing_boxscore <- manifest$game_id[
      manifest$scraped & (is.na(manifest$has_boxscore) | !manifest$has_boxscore)
    ]
    if (length(missing_boxscore) > 0) {
      sample_ids <- utils::head(missing_boxscore, 10)
      stop(
        sprintf(
          "Boxscore missing for %d scraped games (sample: %s).",
          length(missing_boxscore),
          paste(sample_ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (isTRUE(require_betting)) {
    missing_betting <- manifest$game_id[
      manifest$scraped & (is.na(manifest$has_betting) | !manifest$has_betting)
    ]
    if (length(missing_betting) > 0) {
      sample_ids <- utils::head(missing_betting, 10)
      stop(
        sprintf(
          "Betting data missing for %d scraped games (sample: %s).",
          length(missing_betting),
          paste(sample_ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(manifest)
}

#' Build a manifest and write missing data reports
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @param raw_dir Directory to store raw JSON
#' @param check_boxscore TRUE to check if boxscore data exists
#' @param check_betting TRUE to check if betting data exists
