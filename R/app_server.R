
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import bs4Dash
#' @noRd
app_server <- function(input, output, session) {
  # Navbar  #################################################
  observeEvent(input$about, {
    showModal(
      modalDialog(title = "About",
                  div(strong("Created by: "), "Tyler Pollard"),
                  div(strong("Version: "), "1.0"),
                  div(strong("Release Date: "), "27 July 2021"))
    )
  })

  # Data Tabs ################################################
  ## Standings Tab ##########################################
  ### Table Data ----
  mod_standings_server(
    "standings",
    teams_data = teams_data
  )
}
