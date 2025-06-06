#' Helper: Build a GitHub Raw URL for Data Files
#'
#' Constructs the full raw.githubusercontent.com URL for a data file in your repo.
#'
#' @param filename The filename (e.g. "season_standings_2023.rds").
#' @param base_url The base GitHub raw URL. Default is your nfl-analysis-app main branch.
#'
#' @return A character string containing the full URL.
#' @export
#' @noRd
build_github_raw_url <- function(
    filename,
    base_url = "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data"
) {
  file.path(base_url, filename)
}

#' Helper: Validate Seasons Argument
#'
#' Checks that seasons is a numeric vector, non-empty, with no NAs.
#'
#' @param seasons Vector of seasons (years).
#' @return Invisibly TRUE if valid, error otherwise.
#' @export
#' @noRd
validate_seasons <- function(seasons) {
  if (!is.numeric(seasons) || any(is.na(seasons)) || length(seasons) == 0) {
    stop("Seasons argument must be a non-empty numeric vector with no NAs.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Helper: Load Multiple RDS Files by URL with nflreadr
#'
#' Given a vector of full URLs, load and bind as data frame.
#'
#' @param urls Character vector of full URLs.
#' @return A data.frame or tibble row-bound from all RDS files.
#' @export
#' @noRd
load_and_bind_rds_urls <- function(urls) {
  dfs <- lapply(urls, nflreadr::rds_from_url)
  dplyr::bind_rows(dfs)
}
