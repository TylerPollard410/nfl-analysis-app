# mod_standings.R
library(shiny)
library(bs4Dash)  # Changed from shinydashboard to bs4Dash
library(reactable)
library(dplyr)
library(shinyWidgets)
library(shinycssloaders)

# ---- Child module: Standings Table ----
standingsTableOutput <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("standingsTableUI"))
  )
}

standingsTableServer <- function(
    id,
    standingsSeason,
    standingsStat = NULL,
    teams_data,
    standingsTableData,
    conference = NULL,   # NULL means NFL-wide table
    type = c("regular", "playoff", "nfl")
) {
  type <- match.arg(type)
  moduleServer(id, function(input, output, session) {

    # Get table data for this card
    tableDataReact <- reactive({
      req(standingsTableData(), standingsSeason())
      data <- standingsTableData()
      if (!is.null(conference)) data <- data |> filter(team_conf == conference)
      if (type == "regular" || type == "nfl") {
        cols <- if (type == "regular") {
          c("team_division", "team_logo_espn", "team", "div_rank", "GP", "W", "L", "T", "W-L%", "PF", "team_PPG", "PA", "opp_PPG", "PD", "MOV", "SOS", "SRS", "OSRS", "DSRS")
        } else {
          c("team_logo_espn", "team", "conf_rank", "nfl_rank", "GP", "W", "L", "T", "W-L%", "PF", "PA", "PD", "MOV", "SOS", "SRS", "OSRS", "DSRS")
        }
        data <- data |> select(any_of(cols))
        if ("div_rank" %in% names(data)) data <- data |> arrange(div_rank)
        if ("conf_rank" %in% names(data)) data <- data |> arrange(conf_rank)
        if ("nfl_rank" %in% names(data)) data <- data |> arrange(nfl_rank)
        data
      } else {
        data |> select(
          seed, team_logo_espn, team_name, GP, W, L, T, `W-L%`, `CON%`, `DIV%`
        ) |> arrange(seed, desc(`W-L%`), desc(`CON%`), desc(`DIV%`))
      }
    })

    output$standingsTable <- renderReactable({
      req(tableDataReact())
      data <- tableDataReact()
      if (type == "regular") {
        stat <- if (!is.null(standingsStat())) standingsStat() else "Total"
        show_data <- if (stat == "Total") data |> select(-c(team_PPG, opp_PPG)) else data |> select(-c(PF, PA))
        reactable(
          show_data,
          theme = fivethirtyeight(centered = TRUE, header_font_size = "0.9em", font_size = "1.0em"),
          highlight = TRUE, compact = TRUE, pagination = FALSE, wrap = FALSE,
          outlined = FALSE, bordered = FALSE, sortable = FALSE, showSortable = FALSE,
          fullWidth = TRUE,
          rowStyle = if ("team_division" %in% names(data)) group_border_sort(columns = "team_division",
                                                                             border_color = "black", border_width = "1.5px", border_style = "solid") else NULL,
          defaultColGroup = colGroup(headerStyle = list(border = "none")),
          columnGroups = if ("team_division" %in% names(data)) list(
            colGroup(name = "", columns = c("team_division"), headerClass = "no-division-underline"),
            colGroup(name = "Record", columns = c("GP", "W", "L", "T", "W-L%")),
            colGroup(name = "Points", columns = if (stat == "Total") c("PF", "PA", "PD") else c("team_PPG", "opp_PPG", "PD")),
            colGroup(name = "Performance", columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"))
          ) else NULL,
          defaultColDef = colDef(vAlign = "center", minWidth = 50,
                                 headerStyle = list(borderTop = "none", paddingTop = "3px")),
          columns = list(
            team_division = colDef(name = "", minWidth = 90, style = group_merge_sort("team_division")),
            team_logo_espn = colDef(name = "", maxWidth = 35, sticky = "left", cell = embed_img(width = "30px", height = "30px")),
            team = colDef(name = "Team", maxWidth = 60, style = list(borderRight = "1px solid black")),
            GP = colDef(name = "GP", minWidth = 40, align = "center", style = list(borderRight = "1px solid #d3d3d3")),
            W = colDef(name = "W", align = "center", minWidth = 30),
            L = colDef(name = "L", align = "center", minWidth = 30),
            T = colDef(name = "T", align = "center", minWidth = 30),
            `W-L%` = colDef(name = "W-L%", format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 60, style = list(borderRight = "1px solid #d3d3d3")),
            PD = colDef(align = "center", style = list(borderRight = "1px solid #d3d3d3")),
            MOV = colDef(format = colFormat(digits = 2)),
            SOS = colDef(format = colFormat(digits = 2)),
            SRS = colDef(format = colFormat(digits = 2),
                         style = color_scales(data = data, colors = c("red", "pink", "whitesmoke", "palegreen", "green"), bias = 1, brighten_text = FALSE)),
            OSRS = colDef(format = colFormat(digits = 2)),
            DSRS = colDef(format = colFormat(digits = 2)),
            conf_rank = colDef(name = "Conf", minWidth = 40, align = "center"),
            nfl_rank  = colDef(name = "NFL", minWidth = 40, align = "center")
          )
        )
      } else if (type == "nfl") {
        reactable(
          data,
          theme = fivethirtyeight(centered = TRUE, header_font_size = "0.9em", font_size = "1.0em"),
          highlight = TRUE, compact = TRUE, pagination = FALSE, wrap = FALSE,
          outlined = FALSE, bordered = FALSE, sortable = FALSE, showSortable = FALSE,
          fullWidth = TRUE,
          defaultColDef = colDef(vAlign = "center", minWidth = 50, headerStyle = list(borderTop = "none", paddingTop = "3px")),
          columns = list(
            team_logo_espn = colDef(name = "", maxWidth = 35, sticky = "left", cell = embed_img(width = "30px", height = "30px")),
            team = colDef(name = "Team", maxWidth = 60, style = list(borderRight = "1px solid black")),
            conf_rank = colDef(name = "Conf", minWidth = 40, align = "center"),
            nfl_rank  = colDef(name = "NFL", minWidth = 40, align = "center"),
            GP = colDef(name = "GP", minWidth = 40, align = "center", style = list(borderRight = "1px solid #d3d3d3")),
            W = colDef(name = "W", align = "center", minWidth = 30),
            L = colDef(name = "L", align = "center", minWidth = 30),
            T = colDef(name = "T", align = "center", minWidth = 30),
            `W-L%` = colDef(name = "W-L%", format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 60, style = list(borderRight = "1px solid #d3d3d3")),
            PD = colDef(align = "center", style = list(borderRight = "1px solid #d3d3d3")),
            MOV = colDef(format = colFormat(digits = 2)),
            SOS = colDef(format = colFormat(digits = 2)),
            SRS = colDef(format = colFormat(digits = 2),
                         style = color_scales(data = data, colors = c("red", "pink", "whitesmoke", "palegreen", "green"), bias = 1, brighten_text = FALSE)),
            OSRS = colDef(format = colFormat(digits = 2)),
            DSRS = colDef(format = colFormat(digits = 2))
          )
        )
      } else { # playoff
        reactable(
          data,
          theme = espn(centered = TRUE, header_font_size = 14, font_size = 14),
          highlight = TRUE, compact = TRUE, pagination = FALSE, wrap = FALSE,
          outlined = FALSE, sortable = FALSE, showSortable = FALSE, fullWidth = TRUE,
          columns = list(
            seed = colDef(name = "Seed", align = "center", minWidth = 50, sticky = "left"),
            team_logo_espn = colDef(name = "", minWidth = 30, sticky = "left", cell = embed_img(height = "25px")),
            team_name = colDef(name = "Team", minWidth = 150, style = list(borderRight = "1px solid black")),
            GP = colDef(name = "GP", align = "center", minWidth = 30, style = list(borderRight = "1px solid #d3d3d3")),
            W = colDef(name = "W", align = "center", minWidth = 30),
            L = colDef(name = "L", align = "center", minWidth = 30),
            T = colDef(name = "T", align = "center", minWidth = 30, style = list(borderRight = "1px solid #d3d3d3")),
            `W-L%` = colDef(name = "W-L%", format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 50, style = list(borderRight = "1px solid #d3d3d3")),
            `CON%` = colDef(name = "CON%", format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 50, style = list(borderRight = "1px solid #d3d3d3")),
            `DIV%` = colDef(name = "DIV%", format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 50)
          )
        )
      }
    })

    output$standingsTableUI <- renderUI({
      ns <- session$ns
      req(standingsSeason(), teams_data)

      conf_logo <- if (!is.null(conference)) {
        teams_data |> filter(team_conf == conference) |> pull(team_conference_logo) |> unique()
      } else {
        NULL
      }
      table_title <- switch(
        type,
        "regular" = if (!is.null(conference)) paste(conference, "Standings") else "NFL Standings",
        "playoff" = if (!is.null(conference)) paste(conference, "Playoff Standings") else "NFL Playoffs",
        "nfl"     = "NFL Standings"
      )
      tagList(
        tags$style(HTML(".no-division-underline.rt-th-group:after {display: none !important;} .card-body { padding: 0px }")),
        bs4Card(  # Changed from box to bs4Card for bs4Dash compatibility
          title = div(
            style = "display: flex; align-items: center;",
            if (!is.null(conf_logo)) img(src = conf_logo, style = "height: 25px;"),
            strong(table_title, style = "margin-left: 6px; font-size: 25px;"),
            strong(standingsSeason(), style = "margin-left: 6px; font-size: 20px;")
          ),
          width = 12,
          status = "primary",
          withSpinner(
            reactableOutput(ns("standingsTable")),
            type = 8
          )
        )
      )
    })
  })
}

# ---- Parent module: Standings Tab ----
mod_standings_ui <- function(id) {
  tagList(
    fluidRow(
      div(style = "margin-right: 1rem",
          virtualSelectInput(
            inputId = NS(id, "season"),
            label = "Select season",
            choices = seq(2007, get_current_season()),
            selected = get_current_season()
          )
      ),
      div(style = "margin-right: 1rem",
          radioGroupButtons(
            inputId = NS(id, "stat"),
            label = "Table Statistic",
            choices = c("Total", "Game"),
            status = "info"
          )
      ),
      div(style = "margin-right: 1rem",
          radioGroupButtons(
            inputId = NS(id, "rank_type"),
            label = "Rank Type",
            choices = c("Division" = "div_rank", "Conference" = "conf_rank", "NFL" = "nfl_rank"),
            selected = "div_rank",
            status = "info"
          )
      )
    ),
    br(),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'nfl_rank'", NS(id, "rank_type")),
      standingsTableOutput(NS(id, "nflTable"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'nfl_rank'", NS(id, "rank_type")),
      fluidRow(
        column(6, standingsTableOutput(NS(id, "afcReg"))),
        column(6, standingsTableOutput(NS(id, "nfcReg")))
      ),
      fluidRow(
        column(6, standingsTableOutput(NS(id, "afcPlayoff"))),
        column(6, standingsTableOutput(NS(id, "nfcPlayoff")))
      )
    )
  )
}

mod_standings_server <- function(id, teams_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    standingsSeason <- reactive({
      req(input$season)
      as.numeric(input$season)
    })

    standingsStat <- reactive({
      req(input$stat)
      input$stat
    })

    rankType <- reactive({
      req(input$rank_type)
      input$rank_type
    })

    selected_season_data <- reactive({
      req(standingsSeason(), teams_data)
      tryCatch({
        standings_df <- load_season_standings_data(seasons = standingsSeason())
        standings_df |>
          left_join(
            teams_data |> select(team_abbr, team_logo_espn, team_name),
            by = join_by(team == team_abbr)
          ) |>
          mutate(
            team_conf = conf,
            team_division = division,
            GP = games,
            W = wins,
            L = losses,
            T = ties,
            `W-L%` = win_pct,
            `CON%` = conf_pct,
            `DIV%` = div_pct,
            PF = pf,
            PA = pa,
            PD = pd,
            MOV = MOV,
            SOS = SOS,
            SRS = SRS,
            OSRS = OSRS,
            DSRS = DSRS,
            team_PPG = pf / games,
            opp_PPG = pa / games,
            seed = conf_rank  # For playoff tables
          ) |>
          relocate(team_logo_espn, .after = team_division)
      }, error = function(e) {
        message("Error in selected_season_data: ", e$message)
        NULL
      })
    })

    nfl_data <- reactive({
      req(selected_season_data())
      data <- selected_season_data()
      data$nfl_rank <- rank(-data$SRS, ties.method = "min")
      data
    })

    standingsTableServer(
      id = ns("afcReg"),
      standingsSeason = standingsSeason,
      standingsStat   = standingsStat,
      teams_data      = teams_data,
      standingsTableData = selected_season_data,
      conference      = "AFC",
      type           = "regular"
    )
    standingsTableServer(
      id = ns("nfcReg"),
      standingsSeason = standingsSeason,
      standingsStat   = standingsStat,
      teams_data      = teams_data,
      standingsTableData = selected_season_data,
      conference      = "NFC",
      type           = "regular"
    )
    standingsTableServer(
      id = ns("afcPlayoff"),
      standingsSeason = standingsSeason,
      standingsStat   = NULL,
      teams_data      = teams_data,
      standingsTableData = selected_season_data,
      conference      = "AFC",
      type           = "playoff"
    )
    standingsTableServer(
      id = ns("nfcPlayoff"),
      standingsSeason = standingsSeason,
      standingsStat   = NULL,
      teams_data      = teams_data,
      standingsTableData = selected_season_data,
      conference      = "NFC",
      type           = "playoff"
    )
    standingsTableServer(
      id = ns("nflTable"),
      standingsSeason = standingsSeason,
      standingsStat   = standingsStat,
      teams_data      = teams_data,
      standingsTableData = nfl_data,
      conference      = NULL,
      type           = "nfl"
    )
  })
}
