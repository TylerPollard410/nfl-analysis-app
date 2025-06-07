library(shiny)
library(shinycssloaders)
library(reactable)
library(bs4Dash)
# Source your mod_standings.R (edit path if needed)
#source("R/mod_standings.R")   # or devtools::load_all() if using as a package
devtools::load_all()

ui <- bs4DashPage(
  dark = NULL,
  header = dashboardHeader(title = "Standings Test"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Standings", tabName = "standings_tab", icon = icon("table"))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "standings_tab",
        mod_standings_ui("standings")
      )
    )
  )
)

server <- function(input, output, session) {
  mod_standings_server("standings", teams_data)
}

shinyApp(ui, server)
