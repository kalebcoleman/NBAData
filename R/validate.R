#' Validate parsed tables for data quality checks
#'
#' @param tables Named list from espn_nba_parse_raw_dir()
#' @param require_completed_only Only validate completed games when TRUE
#' @param warn_only Emit warnings instead of stopping on issues
#' @return A list with ok, issues, missingness, and samples
#' @export
validate_parsed_tables <- function(tables,
                                   require_completed_only = TRUE,
                                   warn_only = FALSE) {
  games <- tables$games
  team_box <- tables$team_box
  player_box <- tables$player_box

  if (is.null(games)) games <- .espn_nba_empty_table("games")
  if (is.null(team_box)) team_box <- .espn_nba_empty_table("team_box")
  if (is.null(player_box)) player_box <- .espn_nba_empty_table("player_box")

  completed_ids <- unique(games$game_id)
  if (isTRUE(require_completed_only) && "status_completed" %in% names(games)) {
    completed_ids <- unique(games$game_id[games$status_completed %in% TRUE])
  }
  completed_ids <- suppressWarnings(as.integer(completed_ids))
  completed_ids <- completed_ids[!is.na(completed_ids) & completed_ids > 0]

  issues <- tibble::tibble(
    issue = character(),
    game_id = integer(),
    detail = character(),
    count = integer()
  )

  if (length(completed_ids) > 0) {
    team_counts <- dplyr::count(team_box, game_id, name = "team_rows")
    team_counts <- dplyr::filter(team_counts, !is.na(game_id))
    expected <- tibble::tibble(game_id = completed_ids)
    team_counts <- dplyr::left_join(expected, team_counts, by = "game_id")
    team_counts$team_rows[is.na(team_counts$team_rows)] <- 0L
    bad_team <- dplyr::filter(team_counts, team_rows != 2 & !is.na(game_id))
    if (nrow(bad_team) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          bad_team,
          game_id = game_id,
          issue = "team_box_row_count",
          detail = "Expected 2 team rows",
          count = team_rows
        )
      )
    }

    player_counts <- dplyr::count(player_box, game_id, name = "player_rows")
    player_counts <- dplyr::filter(player_counts, !is.na(game_id))
    player_counts <- dplyr::left_join(expected, player_counts, by = "game_id")
    player_counts$player_rows[is.na(player_counts$player_rows)] <- 0L
    bad_player <- dplyr::filter(player_counts, player_rows <= 0 & !is.na(game_id))
    if (nrow(bad_player) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          bad_player,
          game_id = game_id,
          issue = "player_box_row_count",
          detail = "Expected >0 player rows",
          count = player_rows
        )
      )
    }
  }

  if (nrow(team_box) > 0) {
    dup_team <- dplyr::count(team_box, game_id, team_id, name = "dup_count")
    dup_team <- dplyr::filter(dup_team, dup_count > 1)
    if (nrow(dup_team) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          dup_team,
          game_id = game_id,
          issue = "team_box_duplicate",
          detail = "Duplicate (game_id, team_id)",
          count = dup_count
        )
      )
    }
  }

  if (nrow(player_box) > 0) {
    dup_player <- dplyr::count(player_box, game_id, athlete_id, name = "dup_count")
    dup_player <- dplyr::filter(dup_player, dup_count > 1)
    if (nrow(dup_player) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          dup_player,
          game_id = game_id,
          issue = "player_box_duplicate",
          detail = "Duplicate (game_id, athlete_id)",
          count = dup_count
        )
      )
    }
  }

  issues <- dplyr::filter(issues, !is.na(game_id))

  missingness <- list(
    games = .espn_nba_missingness_report(games),
    team_box = .espn_nba_missingness_report(team_box),
    player_box = .espn_nba_missingness_report(player_box)
  )

  samples <- .espn_nba_sample_games(games)

  ok <- nrow(issues) == 0
  if (!ok) {
    issue_counts <- dplyr::count(issues, issue, name = "n")
    summary <- paste(
      paste0(issue_counts$issue, "=", issue_counts$n),
      collapse = ", "
    )
    sample_rows <- utils::head(issues, 5)
    sample_text <- paste(
      sprintf("%s game_id=%s count=%s", sample_rows$issue, sample_rows$game_id, sample_rows$count),
      collapse = "; "
    )
    msg <- sprintf("Parsed table validation failed: %s. Sample: %s", summary, sample_text)

    if (isTRUE(warn_only)) {
      warning(msg, call. = FALSE)
    } else {
      stop(msg, call. = FALSE)
    }
  }

  list(
    ok = ok,
    issues = issues,
    missingness = missingness,
    samples = samples
  )
}

.espn_nba_missingness_report <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(
      column = names(df),
      n_missing = integer(length(names(df))),
      pct_missing = rep(NA_real_, length(names(df)))
    ))
  }

  n <- nrow(df)
  tibble::tibble(
    column = names(df),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_missing = vapply(df, function(x) sum(is.na(x)) / n, numeric(1))
  )
}

.espn_nba_sample_games <- function(games) {
  if (is.null(games) || nrow(games) == 0 || !"game_date" %in% names(games)) {
    return(list(early = integer(), mid = integer(), late = integer()))
  }

  dates <- sort(unique(stats::na.omit(games$game_date)))
  if (length(dates) == 0) {
    return(list(early = integer(), mid = integer(), late = integer()))
  }

  early_date <- dates[1]
  mid_date <- dates[ceiling(length(dates) / 2)]
  late_date <- dates[length(dates)]

  list(
    early = unique(games$game_id[games$game_date == early_date]),
    mid = unique(games$game_id[games$game_date == mid_date]),
    late = unique(games$game_id[games$game_date == late_date])
  )
}
