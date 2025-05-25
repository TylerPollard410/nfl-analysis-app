#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "nflanalysisapp")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}


# ----------------------------------------------------------------------------
# Global app configuration: data pickers and theme
# ----------------------------------------------------------------------------

#' Precomputed team picker choices
#'
#' A data.frame of team abbreviations, names, conference, and division for the NFL.
#' Used to populate select inputs in the UI.
#'
#' @source nflreadr::load_teams(current = TRUE)
#' @noRd
teams_picker_choices <- nflreadr::load_teams(current = TRUE) |>
  dplyr::select(team_abbr, team_name, team_conf, team_division) |>
  dplyr::arrange(team_division, team_name) |>
  as.data.frame()

#' Dashboard theme for bs4Dash
#'
#' A fresh theme object defining colors and styling for the dashboard.
#' @noRd
my_theme <- fresh::create_theme(
  theme = "paper",
  fresh::bs4dash_sidebar_dark(bg = "#2d3b4d"),
  fresh::bs4dash_status(primary = "purple", info = "#2eec900")
)
