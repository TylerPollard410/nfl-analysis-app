# R/fct_load_scores_data.R

#' Load Scores Data for One or More Seasons
#'
#' Retrieves and combines scores data for the specified `seasons`. Uses cached
#' downloads via `get_season_dataset()` internally.
#'
#' @param seasons Numeric or character vector. One or more seasons
#'   (e.g., `2020`, `2021`). Defaults to 2006 through the most recent season.
#'
#' @return A tibble or data.frame with scores data for the requested seasons.
#'
#' @importFrom nflreadr most_recent_season
#' @export
#' @noRd
load_scores_data <- function(seasons = 2006:most_recent_season()) {
  # Validate seasons (relies on validate_seasons from utils_build_url.R)
  validate_seasons(seasons)
  # Retrieve combined data (delegates to get_season_dataset)
  get_season_dataset("scores_data", seasons)
}
