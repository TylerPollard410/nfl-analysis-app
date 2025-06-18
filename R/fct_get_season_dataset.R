# R/fct_get_season_dataset.R

#' Retrieve and Combine Season Data with Memoised Caching
#'
#' @description
#' Loads and combines data for one or more seasons of a specified dataset, using a memoised
#' backend to cache results in memory. Each season's data is fetched only once per app instance;
#' subsequent requests are served from cache. All loaded seasons are row-bound together.
#'
#' @param dataset Character. Name of dataset folder (e.g., "season_standings_data").
#' @param seasons Integer or character vector. One or more seasons (e.g., 2022, 2023).
#'
#' @return A tibble or data.frame combining rows for all requested seasons.
#'
#' @examples
#' \dontrun{
#'   # Fetch and combine standings for 2022 and 2023
#'   df <- get_season_dataset("season_standings_data", c(2022, 2023))
#' }
#'
#' @export
#' @noRd
get_season_dataset <- function(dataset, seasons) {
  # 1) Validate inputs
  if (!is.character(dataset) || length(dataset) != 1) {
    stop("`dataset` must be a single character string.", call. = FALSE)
  }
  validate_seasons(seasons)   # from utils_build_url.R

  # 2) Fetch and cache each season, handling errors
  data_list <- lapply(seasons, function(season) {
    season_str <- as.character(season)
    tryCatch(
      fetch_one_season_memo(dataset, season_str),
      error = function(e) {
        stop(
          sprintf("Failed to fetch '%s' for season %s: %s", dataset, season_str, conditionMessage(e)),
          call. = FALSE
        )
      }
    )
  })

  # 3) Combine all season data into one tibble/data.frame
  dplyr::bind_rows(data_list)
}
