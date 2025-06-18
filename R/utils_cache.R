# R/utils_cache.R

#' Global App Data Cache (Memoised, LRU)
#'
#' @description
#' Provides a global, process-wide cache for memoising data fetch operations throughout the app,
#' using an LRU (least recently used) eviction strategy.
#'
#' @details
#' The cache is implemented via \code{cachem::cache_mem()}, with a configurable maximum number
#' of objects (default: 20 via \code{getOption("nflapp.cache.max_objects")}). Used as the backend
#' for all memoised data loaders, enabling efficient in-memory caching and automatic eviction.
#'
#' @return An object of class \code{cache_mem}, with methods like \code{$set()}, \code{$get()},
#'   \code{$reset()}, and \code{$keys()}.
#'
#' @seealso \code{\link[memoise]{memoise}}, \code{\link[cachem]{cache_mem}}
#'
#' @keywords internal
#' @noRd
get_app_cache <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- cachem::cache_mem(
        max_n = getOption("nflapp.cache.max_objects", 20),
        evict = "lru"
      )
    }
    cache
  }
})

#' Memoised Season Data Loader
#'
#' @description
#' Fetches a single season/dataset combination, memoised in the global app cache.
#' Intended as the backend for all season-based data loading.
#'
#' @param dataset Character. Dataset name (e.g., "season_standings_data").
#' @param season Integer or character. Year of season.
#'
#' @return Data frame or tibble for the specified dataset and season.
#'
#' @keywords internal
#' @noRd
fetch_one_season <- function(dataset, season) {
  season_str <- as.character(season)
  data_url <- build_data_url(dataset, season_str)
  message(sprintf("Downloading data for '%s' season %s...", dataset, season_str))
  download_and_read_rds(data_url)
}

# Memoise the season loader with the global cache (LRU)
fetch_one_season_memo <- memoise::memoise(fetch_one_season, cache = get_app_cache())

#' Clear All Cached Data
#'
#' @description
#' Empties the global app cache, freeing all memory used by cached datasets.
#' Useful for admin/debugging or memory management.
#'
#' @export
#' @noRd
clear_app_cache <- function() {
  get_app_cache()$reset()
  message("App cache cleared.")
}

#' List Cached Keys
#'
#' @description
#' Returns a character vector of all keys currently stored in the global app cache.
#'
#' @return Character vector of cache keys.
#'
#' @export
#' @noRd
cache_keys <- function() {
  get_app_cache()$keys()
}
