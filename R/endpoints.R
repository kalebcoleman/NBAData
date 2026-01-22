# ESPN NBA endpoint wrappers

#' Get ESPN NBA game summary raw JSON
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A list containing the raw summary JSON
#' @export
espn_nba_summary_raw <- function(game_id, save_raw = FALSE, raw_dir = "data/raw") {
  result <- espn_nba_summary_raw_safe(game_id, save_raw = save_raw, raw_dir = raw_dir)
  if (!is.null(result$error)) {
    stop(
      sprintf(
        "ESPN summary request failed (status %s): %s",
        result$status_code,
        result$error
      ),
      call. = FALSE
    )
  }
  result$raw
}

#' Get ESPN NBA game summary raw JSON with status metadata
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @param times Number of retry attempts
#' @param pause_base Base seconds for exponential backoff
#' @param pause_cap Max seconds for exponential backoff
#' @return A list containing status_code, raw, and error
#' @export
espn_nba_summary_raw_safe <- function(game_id,
                                      save_raw = FALSE,
                                      raw_dir = "data/raw",
                                      times = 5,
                                      pause_base = 1,
                                      pause_cap = 60) {
  res <- .espn_nba_summary_response_raw(
    game_id,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap
  )
  status_code <- httr::status_code(res)
  if (is.na(status_code) || status_code < 200 || status_code >= 300) {
    message <- httr::http_status(res)$message
    return(list(status_code = status_code, raw = NULL, error = message))
  }

  raw_summary <- tryCatch(
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")),
    error = function(e) e
  )
  if (inherits(raw_summary, "error")) {
    return(list(status_code = status_code, raw = NULL, error = conditionMessage(raw_summary)))
  }

  if (isTRUE(save_raw)) {
    season <- .espn_nba_extract_summary_season(raw_summary)
    game_date <- .espn_nba_clean_date_key(.espn_nba_extract_summary_date(raw_summary))
    key <- .espn_nba_summary_key(season = season, game_date = game_date, game_id = game_id)
    save_raw_json(raw_summary, "summary", key, dir = raw_dir)
  }

  list(status_code = status_code, raw = raw_summary, error = NULL)
}

#' Check if a summary payload has boxscore data
#'
#' @param raw_summary Summary JSON list
#' @return TRUE if team and player stats are present
#' @export
espn_nba_summary_has_boxscore <- function(raw_summary) {
  has_team_stats <- !is.null(raw_summary$boxscore$teams) &&
    length(raw_summary$boxscore$teams) == 2 &&
    length(raw_summary$boxscore$teams[[1]]$statistics) > 0

  has_player_stats <- !is.null(raw_summary$boxscore$players) &&
    length(raw_summary$boxscore$players) > 0 &&
    length(raw_summary$boxscore$players[[1]]$statistics) > 0

  isTRUE(has_team_stats && has_player_stats)
}

#' Check if a summary payload has betting data
#'
#' @param raw_summary Summary JSON list
#' @return TRUE if betting data is present
#' @export
espn_nba_summary_has_betting <- function(raw_summary) {
  has_pickcenter <- "pickcenter" %in% names(raw_summary)
  has_ats <- "againstTheSpread" %in% names(raw_summary)
  has_predictor <- "predictor" %in% names(raw_summary)
  isTRUE(has_pickcenter || has_ats || has_predictor)
}

#' Find a recent completed game with boxscore data
#'
#' @param days_back Number of days to look back
#' @param end_date Date to end the search (defaults to Sys.Date())
#' @return A game_id or NULL if none found
#' @export
espn_nba_find_boxscore_game_id <- function(days_back = 30, end_date = Sys.Date()) {
  end_date <- as.Date(end_date)
  dates <- format(end_date - 1:days_back, "%Y%m%d")
  for (date_str in dates) {
    scoreboard <- espn_nba_scoreboard(date_str)
    if (nrow(scoreboard) == 0) {
      next
    }
    completed <- scoreboard[scoreboard$status_completed %in% TRUE, , drop = FALSE]
    if (nrow(completed) == 0) {
      next
    }

    for (game_id in completed$game_id) {
      raw_summary <- tryCatch(
        espn_nba_summary_raw(game_id),
        error = function(e) NULL
      )
      if (!is.null(raw_summary) && espn_nba_summary_has_boxscore(raw_summary)) {
        return(as.integer(game_id))
      }
    }
  }

  NULL
}

#' Check if a game has boxscore data available
#'
#' @param game_id Game ID
#' @return TRUE if boxscore data is available, FALSE otherwise
#' @export
espn_nba_game_has_boxscore <- function(game_id) {
  raw_summary <- espn_nba_summary_raw(game_id)
  espn_nba_summary_has_boxscore(raw_summary)
}

.espn_nba_expand_dates <- function(dates) {
  if (length(dates) == 1 && grepl("-", dates)) {
    parts <- strsplit(dates, "-", fixed = TRUE)[[1]]
    start_date <- as.Date(parts[1], format = "%Y%m%d")
    end_date <- as.Date(parts[2], format = "%Y%m%d")
    return(format(seq.Date(start_date, end_date, by = "day"), "%Y%m%d"))
  }

  as.character(dates)
}

#' Get valid game ids for a date range with required data
#'
#' @param dates Vector of dates or a range in YYYYMMDD or YYYYMMDD-YYYYMMDD
#' @param require One or more of "summary", "team", "player", "betting"
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @param quiet Suppress progress messages
#' @return A vector of game ids
#' @export
espn_nba_valid_game_ids <- function(dates,
                                    require = c("team", "player"),
                                    save_raw = FALSE,
                                    raw_dir = "data/raw",
                                    quiet = FALSE) {
  require <- match.arg(require, choices = c("summary", "team", "player", "betting"), several.ok = TRUE)
  dates <- .espn_nba_expand_dates(dates)

  game_ids <- integer()
  for (date_str in dates) {
    scoreboard <- espn_nba_scoreboard(date_str)
    if (nrow(scoreboard) == 0) {
      next
    }
    completed <- scoreboard[scoreboard$status_completed %in% TRUE, , drop = FALSE]
    if (nrow(completed) == 0) {
      next
    }

    for (game_id in completed$game_id) {
      raw_summary <- tryCatch(
        espn_nba_summary_raw(game_id, save_raw = save_raw, raw_dir = raw_dir),
        error = function(e) NULL
      )
      if (is.null(raw_summary)) {
        next
      }

      has_team_player <- espn_nba_summary_has_boxscore(raw_summary)
      has_betting <- espn_nba_summary_has_betting(raw_summary)

      ok <- TRUE
      if ("summary" %in% require) {
        ok <- TRUE
      }
      if ("team" %in% require || "player" %in% require) {
        ok <- isTRUE(ok && has_team_player)
      }
      if ("betting" %in% require) {
        ok <- isTRUE(ok && has_betting)
      }

      if (isTRUE(ok)) {
        game_ids <- c(game_ids, as.integer(game_id))
        if (!isTRUE(quiet)) {
          message(glue::glue("Valid game: {game_id} ({date_str})"))
        }
      }
    }
  }

  unique(game_ids)
}

#' Scrape raw summaries for a date range
#'
#' @param dates Vector of dates or a range in YYYYMMDD or YYYYMMDD-YYYYMMDD
#' @param raw_dir Directory to store raw JSON
#' @param pause_sec Seconds to pause between games
#' @param overwrite Overwrite existing raw files
#' @param quiet Suppress progress messages
#' @return A data frame of scraped game ids
#' @export
espn_nba_scrape_summaries_for_dates <- function(dates,
                                                raw_dir = "data/raw",
                                                pause_sec = 0.25,
                                                overwrite = FALSE,
                                                quiet = FALSE) {
  dates <- .espn_nba_expand_dates(dates)
  scraped <- list()

  for (date_str in dates) {
    scoreboard <- espn_nba_scoreboard(date_str)
    if (nrow(scoreboard) == 0) {
      next
    }

    for (row in seq_len(nrow(scoreboard))) {
      game_id <- scoreboard$game_id[[row]]
      season <- scoreboard$season[[row]]
      game_date <- scoreboard$game_date[[row]]
      game_date_key <- .espn_nba_clean_date_key(game_date)
      key <- .espn_nba_summary_key(season = season, game_date = game_date_key, game_id = game_id)
      path <- file.path(raw_dir, paste0("summary_", key, ".json"))
      if (!isTRUE(overwrite) && file.exists(path)) {
        next
      }

      result <- espn_nba_summary_raw_safe(
        game_id,
        save_raw = TRUE,
        raw_dir = raw_dir
      )
      if (is.null(result$error)) {
        scraped[[length(scraped) + 1]] <- data.frame(
          game_id = as.integer(game_id),
          date = date_str,
          stringsAsFactors = FALSE
        )
        if (!isTRUE(quiet)) {
          message(glue::glue("Saved summary for {game_id} ({date_str})"))
        }
      } else {
        log_failed_scrape(
          game_id = game_id,
          game_date = date_str,
          status_code = result$status_code,
          error_message = result$error,
          dir = raw_dir
        )
      }
      if (pause_sec > 0) {
        Sys.sleep(pause_sec)
      }
    }
  }

  if (length(scraped) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(scraped)
}

#' Scrape raw summaries for a season
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @param raw_dir Directory to store raw JSON
#' @param pause_sec Seconds to pause between games
#' @param overwrite Overwrite existing raw files
#' @param quiet Suppress progress messages
#' @return A data frame of scraped game ids
#' @export
espn_nba_scrape_summaries_for_season <- function(season,
                                                 season_type = c("regular", "postseason", "all"),
                                                 start_date = NULL,
                                                 end_date = NULL,
                                                 raw_dir = "data/raw",
                                                 pause_sec = 0.25,
                                                 overwrite = FALSE,
                                                 quiet = FALSE) {
  games <- espn_nba_season_game_ids(
    season = season,
    season_type = season_type,
    start_date = start_date,
    end_date = end_date
  )

  if (nrow(games) == 0) {
    return(data.frame())
  }

  dates <- unique(format(games$game_date, "%Y%m%d"))
  espn_nba_scrape_summaries_for_dates(
    dates = dates,
    raw_dir = raw_dir,
    pause_sec = pause_sec,
    overwrite = overwrite,
    quiet = quiet
  )
}

#' Scrape raw summaries for a season with progress
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @param raw_dir Directory to store raw JSON
#' @param pause_sec Seconds to pause between games
#' @param overwrite Overwrite existing raw files
#' @param quiet Suppress progress messages
#' @return A data frame of scraped game ids
#' @export
espn_nba_scrape_summaries_for_season_progress <- function(season,
                                                          season_type = c("regular", "postseason", "all"),
                                                          start_date = NULL,
                                                          end_date = NULL,
                                                          raw_dir = "data/raw",
                                                          pause_sec = 0.25,
                                                          overwrite = FALSE,
                                                          quiet = FALSE) {
  season_type <- match.arg(season_type)
  games <- espn_nba_season_game_ids(
    season = season,
    season_type = season_type,
    start_date = start_date,
    end_date = end_date
  )

  if (nrow(games) == 0) {
    return(data.frame())
  }

  total <- nrow(games)
  scraped <- list()
  saved <- 0
  skipped <- 0
  failed <- 0

  for (row in seq_len(total)) {
    game_id <- games$game_id[[row]]
    season_value <- games$season[[row]]
    game_date <- .espn_nba_clean_date_key(games$game_date[[row]])
    key <- .espn_nba_summary_key(season = season_value, game_date = game_date, game_id = game_id)
    path <- file.path(raw_dir, paste0("summary_", key, ".json"))

    if (!isTRUE(overwrite) && file.exists(path)) {
      skipped <- skipped + 1
      if (!isTRUE(quiet)) {
        message(glue::glue("[{row}/{total}] Skipped {game_id} (exists)"))
      }
      next
    }

    result <- espn_nba_summary_raw_safe(
      game_id,
      save_raw = TRUE,
      raw_dir = raw_dir
    )

    if (!is.null(result$error)) {
      failed <- failed + 1
      if (!isTRUE(quiet)) {
        message(glue::glue("[{row}/{total}] Failed {game_id}"))
      }
      log_failed_scrape(
        game_id = game_id,
        game_date = games$game_date[[row]],
        status_code = result$status_code,
        error_message = result$error,
        dir = raw_dir
      )
    } else {
      saved <- saved + 1
      scraped[[length(scraped) + 1]] <- data.frame(
        game_id = as.integer(game_id),
        date = as.character(games$game_date[[row]]),
        stringsAsFactors = FALSE
      )
      if (!isTRUE(quiet)) {
        message(glue::glue("[{row}/{total}] Saved {game_id} (saved={saved}, skipped={skipped}, failed={failed})"))
      }
    }

    if (pause_sec > 0) {
      Sys.sleep(pause_sec)
    }
  }

  if (length(scraped) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(scraped)
}

#' Get ESPN NBA schedule (scoreboard endpoint)
#'
#' @param dates Optional date or date range in YYYYMMDD or YYYYMMDD-YYYYMMDD
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A tibble of games
#' @export
get_nba_schedule <- function(dates = NULL, save_raw = FALSE, raw_dir = "data/raw") {
  result <- get_nba_schedule_safe(
    dates = dates,
    save_raw = save_raw,
    raw_dir = raw_dir
  )

  if (!is.null(result$error)) {
    stop(
      sprintf(
        "ESPN schedule request failed (status %s): %s",
        result$status_code,
        result$error
      ),
      call. = FALSE
    )
  }

  result$data
}

.espn_nba_schedule_from_raw <- function(raw) {
  if (is.null(raw$events) || length(raw$events) == 0) {
    return(tibble::tibble())
  }

  games <- raw$events
  schedule <- flatten_to_tibble(games)
  if (!"gameId" %in% names(schedule) && "id" %in% names(schedule)) {
    schedule$gameId <- schedule$id
  }
  if (!"game_date" %in% names(schedule) && "date" %in% names(schedule)) {
    schedule$game_date <- schedule$date
  }
  schedule
}

#' Get ESPN NBA schedule safely with status metadata
#'
#' @param dates Optional date or date range in YYYYMMDD or YYYYMMDD-YYYYMMDD
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @param times Number of retry attempts
#' @param pause_base Base seconds for exponential backoff
#' @param pause_cap Max seconds for exponential backoff
#' @return A list containing status_code, data, and error
#' @export
get_nba_schedule_safe <- function(dates = NULL,
                                  save_raw = FALSE,
                                  raw_dir = "data/raw",
                                  times = 5,
                                  pause_base = 1,
                                  pause_cap = 60) {
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard")
  req <- httr2::request(url)
  if (!is.null(dates)) {
    req <- httr2::req_url_query(req, dates = dates)
  }

  req <- httr2::req_retry(
    req,
    max_tries = times,
    backoff = function(i) min(pause_base * (2 ^ (i - 1)), pause_cap),
    is_transient = function(resp) {
      status <- httr2::resp_status(resp)
      status %in% c(429, 500, 502, 503, 504)
    }
  )

  res <- tryCatch(
    httr2::req_perform(req),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    return(list(status_code = NA_integer_, data = tibble::tibble(), error = conditionMessage(res)))
  }

  status_code <- httr2::resp_status(res)
  if (is.na(status_code) || status_code < 200 || status_code >= 300) {
    return(list(
      status_code = status_code,
      data = tibble::tibble(),
      error = httr2::resp_status_desc(res)
    ))
  }

  raw <- tryCatch(
    httr2::resp_body_json(res, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(raw, "error")) {
    return(list(status_code = status_code, data = tibble::tibble(), error = conditionMessage(raw)))
  }

  if (isTRUE(save_raw)) {
    key <- if (is.null(dates)) "latest" else dates
    save_raw_json(raw, "scoreboard", key, dir = raw_dir)
  }

  list(status_code = status_code, data = .espn_nba_schedule_from_raw(raw), error = NULL)
}

#' Get ESPN NBA schedule with simple caching
#'
#' @param dates Optional date or date range in YYYYMMDD or YYYYMMDD-YYYYMMDD
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A tibble of games
#' @export
get_nba_schedule_cached <- function(dates = NULL, save_raw = FALSE, raw_dir = "data/raw") {
  key <- if (is.null(dates)) "schedule_latest" else paste0("schedule_", dates)
  get_cached(key, function() get_nba_schedule(dates = dates, save_raw = save_raw, raw_dir = raw_dir))
}

#' Get ESPN NBA scoreboard
#'
#' @param dates Date(s) in YYYYMMDD format
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A data frame of games
#' @export
espn_nba_scoreboard <- function(dates, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  dates <- as.character(dates)
  resp <- .espn_nba_scoreboard_response(dates)
  raw_sched <- jsonlite::fromJSON(
    resp,
    simplifyDataFrame = FALSE,
    simplifyVector = FALSE,
    simplifyMatrix = FALSE
  )
  if (isTRUE(save_raw)) {
    save_raw_json(raw_sched, "scoreboard", dates, dir = raw_dir)
  }

  events <- raw_sched[["events"]]
  if (length(events) == 0) {
    return(data.frame())
  }

  purrr::map_dfr(events, .espn_nba_parse_scoreboard_event)
}

.espn_nba_parse_scoreboard_event <- function(event) {
  competition <- event$competitions[[1]]
  date_str <- competition$date
  date_clean <- sub("Z$", "", date_str)
  game_date_time <- suppressWarnings(lubridate::ymd_hms(date_clean))
  if (is.na(game_date_time)) {
    game_date_time <- suppressWarnings(lubridate::ymd_hm(date_clean))
  }
  game_date_time <- lubridate::with_tz(game_date_time, tzone = "America/New_York")
  game_date <- as.Date(substr(game_date_time, 1, 10))

  season <- event$season$year
  season_type <- event$season$type
  status_type <- competition$status$type
  status_state <- if (!is.null(status_type$state)) as.character(status_type$state) else NA_character_
  status_name <- if (!is.null(status_type$name)) as.character(status_type$name) else NA_character_
  status_completed <- isTRUE(status_type$completed) || identical(status_state, "post")

  competitors <- competition$competitors
  home_idx <- which(vapply(competitors, function(x) x$homeAway, "") == "home")
  away_idx <- which(vapply(competitors, function(x) x$homeAway, "") == "away")
  if (length(home_idx) == 0) home_idx <- 1
  if (length(away_idx) == 0) away_idx <- 2

  home <- competitors[[home_idx[1]]]
  away <- competitors[[away_idx[1]]]

  data.frame(
    game_id = as.integer(competition$id),
    season = as.integer(season),
    season_type = as.integer(season_type),
    game_date = game_date,
    game_date_time = game_date_time,
    home_team_id = as.integer(home$team$id),
    away_team_id = as.integer(away$team$id),
    status_completed = status_completed,
    status_name = status_name,
    status_state = status_state,
    stringsAsFactors = FALSE
  )
}

.espn_nba_default_season_window <- function(season, season_type) {
  season <- as.integer(season)
  season_type <- match.arg(season_type, c("regular", "postseason", "all"))

  if (season_type == "regular") {
    start_date <- as.Date(paste0(season - 1, "-10-15"))
    end_date <- as.Date(paste0(season, "-04-20"))
  } else if (season_type == "postseason") {
    start_date <- as.Date(paste0(season, "-04-10"))
    end_date <- as.Date(paste0(season, "-06-30"))
  } else {
    start_date <- as.Date(paste0(season - 1, "-10-15"))
    end_date <- as.Date(paste0(season, "-06-30"))
  }

  list(start_date = start_date, end_date = end_date)
}

.espn_nba_season_date_range <- function(season, season_type, start_date = NULL, end_date = NULL) {
  defaults <- .espn_nba_default_season_window(season, season_type)

  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
  } else {
    start_date <- defaults$start_date
  }

  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
  } else {
    end_date <- defaults$end_date
  }

  seq.Date(start_date, end_date, by = "day")
}

#' Get ESPN NBA season game ids
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @return A data frame of games with ids
#' @export
espn_nba_season_game_ids <- function(season,
                                     season_type = c("regular", "postseason", "all"),
                                     start_date = NULL,
                                     end_date = NULL) {
  season_type <- match.arg(season_type)

  dates <- .espn_nba_season_date_range(season, season_type, start_date, end_date)
  if (length(dates) == 0) {
    return(data.frame())
  }

  games <- lapply(dates, function(x) {
    espn_nba_scoreboard(format(x, "%Y%m%d"))
  })
  games <- dplyr::bind_rows(games)

  if (nrow(games) == 0) {
    return(games)
  }

  games <- games[games$status_completed %in% TRUE, , drop = FALSE]

  if (season_type == "regular") {
    games <- games[games$season_type == 2, , drop = FALSE]
  } else if (season_type == "postseason") {
    games <- games[games$season_type == 3, , drop = FALSE]
  }

  games[!duplicated(games$game_id), , drop = FALSE]
}

#' Get ESPN NBA betting information
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A named list of data frames: pickcenter, againstTheSpread, predictor
#' @export
espn_nba_betting <- function(game_id, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  raw_summary <- tryCatch(
    espn_nba_summary_raw(game_id, save_raw = save_raw, raw_dir = raw_dir),
    error = function(e) NULL
  )

  if (is.null(raw_summary)) {
    message(glue::glue("{Sys.time()}: Invalid arguments or no betting data available!"))
    return(list(pickcenter = data.frame(), againstTheSpread = data.frame(), predictor = data.frame()))
  }

  .espn_nba_betting_from_raw(raw_summary, game_id)
}

.espn_nba_betting_from_raw <- function(raw_summary, game_id) {
  pickcenter <- data.frame()
  against_the_spread <- data.frame()
  predictor_df <- data.frame()

  if ("pickcenter" %in% names(raw_summary)) {
    pickcenter <- jsonlite::fromJSON(jsonlite::toJSON(raw_summary$pickcenter), flatten = TRUE)
    pickcenter <- janitor::clean_names(pickcenter)
    pickcenter <- dplyr::select(pickcenter, -dplyr::any_of("links"))
    pickcenter <- dplyr::mutate(pickcenter, game_id = as.integer(game_id))
    pickcenter <- dplyr::mutate(
      pickcenter,
      dplyr::across(
        c("provider_id", "away_team_odds_team_id", "home_team_odds_team_id"),
        as.integer
      )
    )
  }

  if ("againstTheSpread" %in% names(raw_summary)) {
    against_the_spread <- jsonlite::fromJSON(jsonlite::toJSON(raw_summary$againstTheSpread))
    against_the_spread <- janitor::clean_names(against_the_spread)
    teams <- dplyr::select(against_the_spread$team, -dplyr::any_of("links"))
    teams <- janitor::clean_names(teams)
    records <- against_the_spread$records

    teams$records <- records
    against_the_spread <- dplyr::mutate(
      teams,
      game_id = as.integer(game_id),
      id = as.integer(id),
      team_id = as.integer(id)
    )
  }

  if ("predictor" %in% names(raw_summary)) {
    predictor_df <- data.frame(
      game_id = as.integer(game_id),
      home_team_id = as.integer(raw_summary$predictor$homeTeam$id),
      away_team_id = as.integer(raw_summary$predictor$awayTeam$id),
      away_team_game_projection = as.numeric(raw_summary$predictor$awayTeam$gameProjection),
      away_team_chance_loss = as.numeric(raw_summary$predictor$awayTeam$teamChanceLoss)
    )
  }

  betting <- c(list(pickcenter), list(against_the_spread), list(predictor_df))
  names(betting) <- c("pickcenter", "againstTheSpread", "predictor")
  betting
}

#' Scrape summary-based data for a single game
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A named list with team, player, betting data frames
#' @export
espn_nba_scrape_game <- function(game_id, save_raw = TRUE, raw_dir = "data/raw") {
  resp <- .espn_nba_summary_response(game_id)
  raw_summary <- jsonlite::fromJSON(resp)

  if (isTRUE(save_raw)) {
    save_raw_json(raw_summary, "summary", game_id, dir = raw_dir)
  }

  team <- tryCatch(helper_espn_nba_team_box(resp), error = function(e) NULL)
  player <- tryCatch(helper_espn_nba_player_box(resp), error = function(e) NULL)
  betting <- tryCatch(.espn_nba_betting_from_raw(raw_summary, game_id), error = function(e) NULL)

  list(team = team, player = player, betting = betting)
}

#' Scrape raw summaries for a season date range
#'
#' @param season Season year
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional season start date override
#' @param end_date Optional season end date override
#' @param require One or more of "team", "player", "betting"
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @param pause_sec Seconds to pause between games
#' @return A data frame of game ids that were scraped
#' @export
espn_nba_scrape_season <- function(season,
                                   season_type = c("regular", "postseason", "all"),
                                   start_date = NULL,
                                   end_date = NULL,
                                   require = c("team", "player"),
                                   save_raw = TRUE,
                                   raw_dir = "data/raw",
                                   pause_sec = 0.25) {
  games <- espn_nba_season_game_ids(
    season = season,
    season_type = season_type,
    start_date = start_date,
    end_date = end_date
  )

  if (nrow(games) == 0) {
    return(data.frame())
  }

  dates <- unique(format(games$game_date, "%Y%m%d"))
  valid_ids <- espn_nba_valid_game_ids(
    dates = dates,
    require = require,
    save_raw = save_raw,
    raw_dir = raw_dir,
    quiet = TRUE
  )

  if (length(valid_ids) == 0) {
    return(data.frame())
  }

  scraped <- lapply(valid_ids, function(game_id) {
    espn_nba_scrape_game(game_id, save_raw = save_raw, raw_dir = raw_dir)
    if (pause_sec > 0) {
      Sys.sleep(pause_sec)
    }
    data.frame(game_id = as.integer(game_id))
  })

  dplyr::bind_rows(scraped)
}

#' Get ESPN NBA team box scores
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A team boxscore data frame
#' @export
espn_nba_team_box <- function(game_id, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  resp <- .espn_nba_summary_response(game_id)
  if (isTRUE(save_raw)) {
    save_raw_json(jsonlite::fromJSON(resp), "summary", game_id, dir = raw_dir)
  }

  team_box_score <- NULL
  tryCatch(
    expr = {
      team_box_score <- helper_espn_nba_team_box(resp)
      if (is.null(team_box_score)) {
        message(glue::glue("{Sys.time()}: No team box score data for {game_id} available!"))
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team box score data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  if (is.null(team_box_score)) {
    raw_summary <- tryCatch(
      jsonlite::fromJSON(resp, simplifyDataFrame = FALSE),
      error = function(e) NULL
    )
    if (!is.null(raw_summary$boxscore$teams)) {
      team_box_score <- jsonlite::fromJSON(
        jsonlite::toJSON(raw_summary$boxscore$teams),
        flatten = TRUE
      )
      team_box_score <- janitor::clean_names(team_box_score)
      team_box_score$game_id <- as.integer(game_id)
    }
  }

  team_box_score
}

#' Get ESPN NBA player box scores
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A player boxscore data frame
#' @export
espn_nba_player_box <- function(game_id, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  resp <- .espn_nba_summary_response(game_id)
  if (isTRUE(save_raw)) {
    save_raw_json(jsonlite::fromJSON(resp), "summary", game_id, dir = raw_dir)
  }

  player_box_score <- NULL
  tryCatch(
    expr = {
      player_box_score <- helper_espn_nba_player_box(resp)
      if (is.null(player_box_score)) {
        message(glue::glue("{Sys.time()}: No player box score data for {game_id} available!"))
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player box score data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  if (is.null(player_box_score)) {
    raw_summary <- tryCatch(
      jsonlite::fromJSON(resp, simplifyDataFrame = FALSE),
      error = function(e) NULL
    )
    if (!is.null(raw_summary$boxscore$players)) {
      player_box_score <- jsonlite::fromJSON(
        jsonlite::toJSON(raw_summary$boxscore$players),
        flatten = TRUE
      )
      player_box_score <- janitor::clean_names(player_box_score)
      player_box_score$game_id <- as.integer(game_id)
    }
  }

  player_box_score
}

#' Get ESPN NBA standings
#'
#' @param season Optional season year
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A standings data frame
#' @export
espn_nba_standings <- function(season = NULL, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  resp <- .espn_nba_standings_response(season)
  raw_standings <- jsonlite::fromJSON(
    resp,
    simplifyDataFrame = FALSE,
    simplifyVector = FALSE,
    simplifyMatrix = FALSE
  )
  if (isTRUE(save_raw)) {
    key <- if (is.null(season)) "current" else season
    save_raw_json(raw_standings, "standings", key, dir = raw_dir)
  }

  entries <- NULL
  if (!is.null(raw_standings$standings) && !is.null(raw_standings$standings$entries)) {
    entries <- raw_standings$standings$entries
  } else if (!is.null(raw_standings$entries)) {
    entries <- raw_standings$entries
  }

  if (length(entries) == 0) {
    return(data.frame())
  }

  standings_df <- jsonlite::fromJSON(jsonlite::toJSON(entries), flatten = TRUE)
  standings_df <- janitor::clean_names(standings_df)

  season_value <- season
  if (is.null(season_value) && !is.null(raw_standings$season$year)) {
    season_value <- raw_standings$season$year
  }
  standings_df$season <- as.integer(season_value)

  standings_df
}
