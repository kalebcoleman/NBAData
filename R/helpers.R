# Helper utilities

fetch_json <- function(url, query = list(), verbose = FALSE, error_on_status = TRUE) {
  req <- httr2::request(url)

  if (length(query)) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  if (isTRUE(verbose)) {
    req <- httr2::req_verbose(req)
  }

  res <- httr2::req_perform(req)

  if (isTRUE(error_on_status) && res$status_code >= 400) {
    stop("HTTP request failed: ", res$status_code, call. = FALSE)
  }

  httr2::resp_body_json(res, simplifyVector = FALSE)
}

fetch_json_response <- function(url, query = list(), verbose = FALSE) {
  req <- httr2::request(url)

  if (length(query)) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  if (isTRUE(verbose)) {
    req <- httr2::req_verbose(req)
  }

  res <- httr2::req_perform(req)
  list(status_code = res$status_code, body = httr2::resp_body_json(res, simplifyVector = FALSE))
}

check_status <- function(res) {
  status <- httr::status_code(res)
  if (is.na(status) || status < 200 || status >= 300) {
    stop(sprintf("ESPN request failed with status %s", status), call. = FALSE)
  }
}

.espn_nba_request <- function(url, times = 5, pause_base = 1, pause_cap = 60) {
  httr::RETRY(
    "GET",
    url,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap
  )
}

.espn_nba_get <- function(url, times = 5, pause_base = 1, pause_cap = 60) {
  res <- .espn_nba_request(url, times = times, pause_base = pause_base, pause_cap = pause_cap)
  check_status(res)
  httr::content(res, as = "text", encoding = "UTF-8")
}

.espn_nba_summary_response_raw <- function(game_id, times = 5, pause_base = 1, pause_cap = 60) {
  summary_url <- "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/summary?"
  full_url <- paste0(summary_url, "event=", game_id)

  .espn_nba_request(full_url, times = times, pause_base = pause_base, pause_cap = pause_cap)
}

.espn_nba_summary_response <- function(game_id, times = 5, pause_base = 1, pause_cap = 60) {
  res <- .espn_nba_summary_response_raw(
    game_id,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap
  )
  check_status(res)
  httr::content(res, as = "text", encoding = "UTF-8")
}

.espn_nba_scoreboard_response <- function(dates) {
  scoreboard_url <- glue::glue(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard?limit=1000&dates={dates}"
  )

  .espn_nba_get(scoreboard_url)
}

.espn_nba_pbp_response <- function(game_id) {
  pbp_url <- glue::glue(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/playbyplay?gameId={game_id}"
  )

  .espn_nba_get(pbp_url)
}

.espn_nba_standings_response <- function(season = NULL) {
  if (is.null(season)) {
    standings_url <- "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/standings"
  } else {
    standings_url <- glue::glue(
      "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/standings?season={season}"
    )
  }

  .espn_nba_get(standings_url)
}

flatten_to_tibble <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null") |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble()
}

save_raw_json <- function(raw, endpoint, key, dir = "data/raw") {
  if (is.null(raw)) {
    return(invisible(NULL))
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  safe_key <- gsub("[^A-Za-z0-9_-]+", "_", as.character(key))
  path <- file.path(dir, paste0(endpoint, "_", safe_key, ".json"))

  jsonlite::write_json(raw, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

.espn_nba_clean_date_key <- function(date_value) {
  if (is.null(date_value)) {
    return(NULL)
  }

  if (inherits(date_value, "Date")) {
    return(format(date_value, "%Y%m%d"))
  }

  date_str <- as.character(date_value)
  date_str <- sub("Z$", "", date_str)
  date_str <- substr(date_str, 1, 10)
  date_str <- gsub("-", "", date_str)

  if (!nzchar(date_str)) {
    return(NULL)
  }

  date_str
}

.espn_nba_summary_key <- function(season = NULL, game_date = NULL, game_id = NULL) {
  parts <- c(as.character(season), as.character(game_date), as.character(game_id))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) {
    return(as.character(game_id))
  }
  paste(parts, collapse = "_")
}

.espn_nba_extract_summary_season <- function(raw_summary) {
  season <- raw_summary$season$year
  if (is.null(season)) {
    season <- raw_summary$header$season$year
  }
  if (is.list(season)) {
    season <- season[[1]]
  } else if (length(season) > 1) {
    season <- season[1]
  }
  if (is.null(season) || is.na(season)) {
    return(NULL)
  }
  as.character(season)
}

.espn_nba_extract_summary_date <- function(raw_summary) {
  date_value <- raw_summary$header$competitions$date
  if (is.null(date_value)) {
    return(NULL)
  }
  if (is.list(date_value)) {
    date_value <- date_value[[1]]
  } else if (length(date_value) > 1) {
    date_value <- date_value[1]
  }
  date_value
}

log_failed_scrape <- function(game_id,
                              game_date,
                              status_code,
                              error_message,
                              dir = "data/raw",
                              filename = "scrape_failures.csv") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, filename)
  entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    game_id = as.integer(game_id),
    game_date = as.character(game_date),
    status_code = as.integer(status_code),
    error_message = as.character(error_message),
    stringsAsFactors = FALSE
  )

  utils::write.table(
    entry,
    path,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(path),
    append = file.exists(path)
  )
  invisible(path)
}
