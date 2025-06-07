# R/fct_load_model_data.R

#' Load Model Data for One or More Seasons
#'
#' Retrieves and combines model data for the specified `seasons`. Uses cached
#' downloads via `get_season_dataset()` internally.
#'
#' @param seasons Numeric or character vector. One or more seasons
#'   (e.g., `2020`, `2021`). Defaults to 2006 through the most recent season.
#'
#' @return A tibble or data.frame with model data for the requested seasons.
#'
#' @importFrom nflreadr most_recent_season
#' @export
#' @noRd
load_model_data <- function(seasons = 2006:most_recent_season()) {
  validate_seasons(seasons)
  get_season_dataset("model_data", seasons)
}
