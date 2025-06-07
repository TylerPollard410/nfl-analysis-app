# R/utils_build_url.R

#' Build a GitHub Raw URL for a Seasonal RDS File
#'
#' Given a dataset folder name under `data/` and a season, constructs the
#' `raw.githubusercontent.com` URL pointing to the RDS file stored in:
#'
#'
#' data/<dataset>/<season>/<dataset>_<season>.rds
#'
#' @param dataset   Character. Folder name under `data/` (e.g., `"season_standings_data"`).
#' @param season    Integer or character. The season (year) (e.g. `2023`).
#' @param base_repo Character. GitHub repository in `"owner/repo"` format.
#'                  Defaults to `"TylerPollard410/nfl-analysis-app"`.
#' @param branch    Character. Branch name where data is stored. Defaults to `"main"`.
#' @param ext       Character. File extension for data files. Defaults to `".rds"`.
#'
#' @return Character. The constructed raw GitHub URL.
#'
#' @examples
#' build_data_url("season_standings_data", 2023)
#' # ->
#' # "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/
#' #  data/season_standings_data/2023/season_standings_data_2023.rds"
#'
#' @export
#' @noRd
build_data_url <- function(dataset,
                           season,
                           base_repo = "TylerPollard410/nfl-analysis-app",
                           branch    = "main",
                           ext       = ".rds") {
  if (!is.character(dataset) || length(dataset) != 1) {
    stop("`dataset` must be a single character string.", call. = FALSE)
  }
  season_str <- as.character(season)
  paste0(
    "https://raw.githubusercontent.com/",
    base_repo, "/", branch, "/",
    "data/", dataset, "/", season_str, "/",
    dataset, "_", season_str, ext
  )
}

#' Validate a Vector of Seasons
#'
#' Checks that `seasons` is a non-empty numeric vector with no `NA` values.
#'
#' @param seasons Numeric vector of seasons (years).
#'
#' @return Invisibly `TRUE` if valid; otherwise throws an error.
#'
#' @keywords internal
#' @noRd
validate_seasons <- function(seasons) {
  if (!is.numeric(seasons) || any(is.na(seasons)) || length(seasons) == 0) {
    stop("`seasons` must be a non-empty numeric vector with no NAs.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Download a Single .rds File to a Tempfile and Read It
#'
#' Internally, some SSL configurations prevent `readRDS(url)` from working.
#' This helper explicitly downloads the file to a temporary location, then calls
#' `readRDS()`.
#'
#' @param url Character. A full URL pointing to a “.rds” file.
#'
#' @return The R object stored in that .rds.
#'
#' @export
#' @noRd
download_and_read_rds <- function(url) {
  if (!is.character(url) || length(url) != 1) {
    stop("`url` must be a single character string pointing to an .rds file.", call. = FALSE)
  }
  temp_file <- tempfile(fileext = ".rds")
  utils::download.file(url, destfile = temp_file, mode = "wb", quiet = TRUE)
  readRDS(temp_file)
}

#' Load and Bind Multiple RDS Files from URLs
#'
#' Given a character vector of full URLs (pointing to `.rds` files), this
#' function downloads each to a temp file via `download_and_read_rds()` and
#' row‐binds them into a single data frame.
#'
#' @param urls Character vector of full URLs to `.rds` files.
#'
#' @return A data.frame or tibble made by row‐binding all loaded RDS objects.
#'
#' @export
#' @noRd
load_and_bind_rds_urls <- function(urls) {
  if (!is.character(urls) || length(urls) == 0) {
    stop("`urls` must be a non-empty character vector.", call. = FALSE)
  }
  dfs <- lapply(urls, download_and_read_rds)
  dplyr::bind_rows(dfs)
}
