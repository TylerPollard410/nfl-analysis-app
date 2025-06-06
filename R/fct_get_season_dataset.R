# R/fct_get_season_dataset.R

#' Get Combined Seasonal Data with Caching
#'
#' Retrieves and combines data for one or more seasons of a given `dataset`.
#' For each season, it:
#'   1. Checks the LRU cache (via `.get_app_data_cache()`) for an existing object.
#'   2. If cached, returns it; otherwise, builds the URL, downloads, reads, caches, and returns it.
#' Finally, all season‐specific data frames are row‐bound into one output.
#'
#' @param dataset Character. Folder name under `data/` (e.g., `"season_standings_data"`).
#' @param seasons Numeric or character vector. One or more seasons (e.g., `2022`, `2023`).
#'
#' @return A tibble or data.frame with rows for all requested seasons, bound together.
#'
#' @examples
#' \dontrun{
#'   # Fetch and combine seasons 2022 and 2023 for “season_standings_data”
#'   df_all <- get_season_dataset("season_standings_data", c(2022, 2023))
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

  # 2) Initialize cache function
  cache_fn <- .get_app_data_cache()  # from utils_cache.R

  # 3) For each season, fetch (with caching)
  data_list <- lapply(seasons, function(s) {
    season_str <- as.character(s)
    key        <- paste0(dataset, "_", season_str)

    # 3a) Check cache
    cached_obj <- cache_fn(key)
    if (!is.null(cached_obj)) {
      return(cached_obj)
    }

    # 3b) Not in cache: build URL, download, read, then cache
    data_url <- build_data_url(dataset, season_str)        # from utils_build_url.R
    message(sprintf("Downloading data for '%s' season %s...", dataset, season_str))
    data_obj <- tryCatch(
      download_and_read_rds(data_url),                     # from utils_build_url.R
      error = function(e) {
        stop(
          sprintf("Failed to fetch '%s' for season %s: %s", dataset, season_str, conditionMessage(e)),
          call. = FALSE
        )
      }
    )
    cache_fn(key, data_obj, set = TRUE)
    data_obj
  })

  # 4) Combine into a single data frame
  dplyr::bind_rows(data_list)
}
