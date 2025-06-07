# R/fct_load_model_data_long.R

#' Load or Compute Model Data (Long Format) for One or More Seasons
#'
#' Either accepts a `model_data` data frame directly or, if `model_data = NULL`,
#' loads `model_data` for the specified `seasons` via `load_model_data()`. The
#' result is reshaped to long format using `clean_homeaway()` and, if
#' `prune_opponent = TRUE`, unwanted `opponent_` columns are removed.
#'
#' @param model_data Data frame or tibble of wide-format model data. If supplied,
#'   this is used directly. Defaults to `NULL`.
#' @param seasons Numeric or character vector. Seasons to load if `model_data` is
#'   `NULL`. Defaults to `most_recent_season()` (a single season).
#' @param prune_opponent Logical(1). If `TRUE` (default), drop any columns
#'   prefixed with `opponent_` except for:
#'   * `opponent`
#'   * `opponent_score`
#'   * `opponent_rest`
#'   * `opponent_moneyline`
#'   * `opponent_moneyline_prob`
#'   * `opponent_spread_odds`
#'   * `opponent_spread_prob`
#'
#' @return A tibble or data.frame in long format with one row per team-game,
#'   including cumulative stats for each team. If `prune_opponent = TRUE`, only
#'   the allowed `opponent_` columns remain.
#'
#' @importFrom nflreadr most_recent_season
#' @importFrom dplyr select
#' @export
#' @noRd
load_model_data_long <- function(model_data = NULL,
                                 seasons = most_recent_season(),
                                 prune_opponent = TRUE) {
  # If no model_data provided, load from seasons
  if (is.null(model_data)) {
    validate_seasons(seasons)
    model_data <- load_model_data(seasons)
  }
  # Reshape to long format
  long_data <- clean_homeaway(model_data, invert = c("result", "spread_line"))
  # Prune opponent_ columns if requested
  if (isTRUE(prune_opponent)) {
    allowed_opponent_cols <- c(
      "opponent",
      "opponent_score",
      "opponent_rest",
      "opponent_moneyline",
      "opponent_moneyline_prob",
      "opponent_spread_odds",
      "opponent_spread_prob"
    )
    to_remove <- names(long_data)[
      startsWith(names(long_data), "opponent_") &
        !(names(long_data) %in% allowed_opponent_cols)
    ]
    long_data <- dplyr::select(long_data, -all_of(to_remove))
  }
  long_data
}
