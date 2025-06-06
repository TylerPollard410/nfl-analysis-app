# R/utils_cache.R

#' Create (and Return) an LRU Cache Closure
#'
#' Returns a function that can store and retrieve objects in an in‐memory cache
#' with a fixed maximum capacity. When the cache exceeds capacity, the least‐
#' recently accessed object is automatically evicted.
#'
#' @param max_size Integer. Maximum number of objects to keep in cache. Defaults to
#'   `getOption("nflapp.cache.max_objects", 20)`. Must be a positive integer.
#'
#' @return A closure with signature `function(key, value = NULL, set = FALSE)`:
#'   - If `set = TRUE` and `value` is non‐NULL, stores `value` under `key`, evicting
#'     the least‐recently‐used item if capacity is exceeded.
#'   - If `set = FALSE`, returns the object stored under `key` (or `NULL` if missing),
#'     and updates internal usage order.
#'
#' @examples
#' cache <- make_lru_cache(3)
#' cache("a", 1L, set = TRUE)
#' cache("b", 2L, set = TRUE)
#' cache("a")          # returns 1, marks "a" as most recently used
#' cache("c", 3L, set = TRUE)
#' cache("d", 4L, set = TRUE)  # evicts "b"
#' cache("b")          # returns NULL
#'
#' @export
#' @noRd
make_lru_cache <- function(max_size = getOption("nflapp.cache.max_objects", 20)) {
  if (!is.numeric(max_size) || length(max_size) != 1 || max_size < 1) {
    stop("`max_size` must be a single positive integer.", call. = FALSE)
  }
  cache_env    <- new.env(parent = emptyenv())
  access_order <- character(0)

  function(key, value = NULL, set = FALSE) {
    if (!is.character(key) || length(key) != 1) {
      stop("`key` must be a single character string.", call. = FALSE)
    }

    # If storing a new value:
    if (isTRUE(set) && !is.null(value)) {
      assign(key, value, envir = cache_env)
      access_order <<- c(setdiff(access_order, key), key)

      if (length(access_order) > max_size) {
        lru_key <- access_order[1]
        rm(list = lru_key, envir = cache_env)
        access_order <<- access_order[-1]
      }
    }

    # If retrieving (or just checking):
    if (exists(key, envir = cache_env, inherits = FALSE)) {
      access_order <<- c(setdiff(access_order, key), key)
      return(get(key, envir = cache_env, inherits = FALSE))
    }

    return(NULL)
  }
}

#' Internal helper: Lazy‐initialize the Global LRU Cache
#'
#' Creates (via `make_lru_cache()`) the LRU cache on first call, then returns
#' the same cache function thereafter.
#'
#' @return Function. The LRU cache accessor/setter (`function(key, value, set)`).
#'
#' @keywords internal
#' @noRd
.get_app_data_cache <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- make_lru_cache(getOption("nflapp.cache.max_objects", 20))
    }
    cache
  }
})

#' Check if a Key Exists in the Global Cache
#'
#' @param key Character. The cache key to look up.
#' @return Logical. `TRUE` if `key` exists, otherwise `FALSE`.
#'
#' @export
#' @noRd
cache_has <- function(key) {
  if (!is.character(key) || length(key) != 1) {
    stop("`key` must be a single character string.", call. = FALSE)
  }
  exists(key, envir = .cache_env, inherits = FALSE)
}

#' Retrieve an Object from the Global Cache
#'
#' @param key Character. The cache key to retrieve.
#' @return The cached object, or `NULL` if not present.
#'
#' @export
#' @noRd
cache_get <- function(key) {
  if (!is.character(key) || length(key) != 1) {
    stop("`key` must be a single character string.", call. = FALSE)
  }
  if (!exists(key, envir = .cache_env, inherits = FALSE)) {
    return(NULL)
  }
  .cache_order <<- c(setdiff(.cache_order, key), key)
  get(key, envir = .cache_env, inherits = FALSE)
}

#' Store an Object in the Global Cache (with Eviction)
#'
#' If the cache exceeds its maximum capacity (`MAX_CACHE_ITEMS`), the least‐
#' recently used item is evicted.
#'
#' @param key Character. The cache key under which to store `value`.
#' @param value Any R object. The object to store in cache.
#'
#' @export
#' @noRd
cache_set <- function(key, value) {
  if (!is.character(key) || length(key) != 1) {
    stop("`key` must be a single character string.", call. = FALSE)
  }
  if (missing(value)) {
    stop("`value` must be provided when setting a cache entry.", call. = FALSE)
  }

  if (exists(key, envir = .cache_env, inherits = FALSE)) {
    assign(key, value, envir = .cache_env)
    .cache_order <<- c(setdiff(.cache_order, key), key)
  } else {
    assign(key, value, envir = .cache_env)
    .cache_order <<- c(.cache_order, key)

    if (length(.cache_order) > MAX_CACHE_ITEMS) {
      lru_key <- .cache_order[1]
      rm(list = lru_key, envir = .cache_env)
      .cache_order <<- .cache_order[-1]
    }
  }
}

#' Clear the Entire Global Cache
#'
#' Removes all cached objects and resets the internal usage order.
#'
#' @export
#' @noRd
cache_clear <- function() {
  rm(list = ls(envir = .cache_env, all.names = TRUE), envir = .cache_env)
  .cache_order <<- character(0)
}

# -----------------------------------------------------------------------------
# Define the truly internal cache environment and order at load time.
# (These three objects are implementation details and should NOT be exported.)
# -----------------------------------------------------------------------------
.cache_env      <- new.env(parent = emptyenv())
.cache_order    <- character(0)
MAX_CACHE_ITEMS <- getOption("nflapp.cache.max_objects", 20)
