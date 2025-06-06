#' Load Season Standings Data for One or More Seasons
#'
#' Loads and combines season standings RDS files from your GitHub repo (with caching).
#'
#' @param seasons Integer or integer vector of seasons to load (e.g. 2023, or c(2022, 2023)).
#' @param base_url Optional. Base URL or local path where RDS files are stored. Defaults to your public GitHub repo.
#'
#' @return A single data frame/tibble with data for all requested seasons, row-bound.
#' @export
#' @noRd
#'
#' @examples
#' load_season_standings_data(2023)
#' load_season_standings_data(c(2022, 2023))
load_season_standings <- function(
    seasons,
    base_url = "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/"
) {
  validate_seasons(seasons)
  filenames <- glue::glue("{season}/season_standings_data_{season}.rds", season = seasons)
  urls <- vapply(filenames, build_github_raw_url, character(1), base_url = base_url)
  load_and_bind_rds_urls(urls)
}
