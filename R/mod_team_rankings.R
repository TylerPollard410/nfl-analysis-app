#' overviewUI <- function(id) {
#'   ns <- NS(id)
#'   withSpinner(
#'     reactable::reactableOutput(ns("teamRankingOverviewTable")),
#'     type = 8
#'   )
#' }
#'
#' overviewServer <- function(id,
#'                            rankings_data,
#'                            season_week_data,
#'                            data,
#'                            team_data){
#'   moduleServer(id, function(input, output, session){
#'
#'     overviewData <- reactive({
#'       data() |>
#'         filter(season == rankingsSeason()) |>
#'         slice_tail(n = 1, by = team) |>
#'         arrange(game_id) |>
#'         select(
#'           team,
#'           team_games
#'           # team_wins,
#'           # team_losses,
#'           # team_ties,
#'           # team_win_pct_cum,
#'           team_elo_pre,
#'           team_off_epa_mean_cum,
#'           team_def_epa_mean_cum,
#'           team_off_epa_sum_cum,
#'           team_def_epa_sum_cum,
#'           team_PFG_cum,
#'           team_PAG_cum,
#'           team_MOV_cum,
#'           team_SOS_cum,
#'           team_SRS_cum,
#'           team_OSRS_cum,
#'           team_DSRS_cum
#'         ) |>
#'         mutate(
#'           team_net_epa_mean = team_off_epa_mean_cum - team_def_epa_mean_cum,
#'           .after = team_def_epa_mean_cum
#'         ) |>
#'         mutate(
#'           team_net_epa_sum = team_off_epa_sum_cum - team_def_epa_sum_cum,
#'           .after = team_def_epa_sum_cum
#'         ) |>
#'         left_join(
#'           season_week_data |>
#'             slice_tail(n = 1, by = team) |>
#'             select(team, games_played, win, loss, tie, win_loss_percent),
#'           by = join_by(team)
#'         ) |>
#'         left_join(
#'           team_data |> select(team_abbr, team_logo_espn),
#'           by = join_by(team == team_abbr)
#'         ) |>
#'         select(team_logo_espn, team,
#'                games_played, win, loss, tie, win_loss_percent,
#'                everything()) |>
#'         rename_with(~str_remove(.x, pattern = "team_"), .cols = -c(team_logo_espn, team)) |>
#'         rename_with(~str_remove(.x, pattern = "_cum"), .cols = everything())
#'     })
#'
#'     output$teamRankingOverviewTable <- renderReactable({
#'       overviewDataReact <- reactable(
#'         data = overviewData(),
#'         theme = fivethirtyeight(
#'           centered = TRUE,
#'           header_font_size = "0.9em",
#'           font_size = "1.0em"
#'         ),
#'         highlight = TRUE,
#'         compact = TRUE,
#'         pagination = FALSE,
#'         wrap = FALSE,
#'         outlined = FALSE,
#'         bordered = FALSE,
#'         striped = TRUE,
#'         sortable = TRUE,
#'         #showSortable = TRUE,
#'         defaultSorted = list("elo" = "desc"),
#'         defaultSortOrder = "desc",
#'         fullWidth = TRUE,
#'         # defaultColGroup = colGroup(
#'         #   headerStyle = list(
#'         #     border = "none"
#'         #   )
#'         # ),
#'         columnGroups = list(
#'           colGroup(name = "Record",
#'                    columns = c("games_played", "win", "loss", "tie", "win_loss_percent")),
#'           colGroup(name = "Elo",
#'                    columns = c("elo")),
#'           colGroup(name = "EPA/Play",
#'                    columns = str_subset(colnames(overviewData()), "epa_mean")),
#'           colGroup(name = "EPA/Game",
#'                    columns = str_subset(colnames(overviewData()), "epa_sum")),
#'           colGroup(name = "Points/Game",
#'                    columns = c("PFG", "PAG", "MOV")),
#'           colGroup(name = "Simple Rating System",
#'                    columns = c("SOS", "SRS", "OSRS", "DSRS"))
#'         ),
#'         defaultColDef = colDef(
#'           vAlign = "center",
#'           minWidth = 60,
#'           align = "center",
#'           format = colFormat(digits = 2),
#'           headerStyle = list(
#'             borderTop = "none",
#'             paddingTop = "3px"
#'           )
#'         ),
#'         columns = list(
#'           ## Team ----
#'           ### Team Logo ----
#'           team_logo_espn = colDef(
#'             name = "",
#'             maxWidth = 40,
#'             sticky = "left",
#'             cell = embed_img(height = "35px", width = "35px")
#'           ),
#'           ### Team Name ----
#'           team = colDef(
#'             name = "Team",
#'             maxWidth = 60,
#'             style = list(borderRight = "1.5px solid black")
#'           ),
#'           ## Record ----
#'           ### Games Played ----
#'           games_played = colDef(
#'             name = "GP",
#'             format = colFormat(digits = 0),
#'             minWidth = 40,
#'             align = "center",
#'             #style = list(borderRight = "1px solid #d3d3d3")
#'           ),
#'           ### Win ----
#'           win = colDef(
#'             name = "W",
#'             align = "center",
#'             minWidth = 30,
#'             format = colFormat(digits = 0)
#'           ),
#'           ### Loss ----
#'           loss = colDef(
#'             name = "L",
#'             align = "center",
#'             minWidth = 30,
#'             format = colFormat(digits = 0)
#'           ),
#'           ### Tie ----
#'           tie = colDef(
#'             name = "T",
#'             align = "center",
#'             minWidth = 30,
#'             format = colFormat(digits = 0)
#'           ),
#'           ### Win Loss Perc ----
#'           win_loss_percent = colDef(
#'             name = "W-L%",
#'             format = colFormat(percent = TRUE, digits = 1),
#'             align = "center",
#'             #minWidth = 50,
#'             style = list(borderRight = "1px solid #d3d3d3")
#'           ),
#'           ## ELO ----
#'           ### ELO ----
#'           elo = colDef(
#'             #minWidth = 70,
#'             format = colFormat(digits = 0),
#'             style = list(borderRight = "1px solid #d3d3d3")
#'           ),
#'           ## EPA/Play ----
#'           off_epa_mean = colDef(
#'             show = FALSE,
#'             name = "Off"
#'           ),
#'           def_epa_mean = colDef(
#'             show = FALSE,
#'             name = "Def"
#'           ),
#'           net_epa_mean = colDef(
#'             show = FALSE,
#'             name = "Net",
#'             style = list(borderRight = "1px solid #d3d3d3")
#'           ),
#'           ## EPA/Game ----
#'           off_epa_sum = colDef(
#'             name = "Off"
#'           ),
#'           def_epa_sum = colDef(
#'             name = "Def"
#'           ),
#'           net_epa_sum = colDef(
#'             name = "Net",
#'             style = list(borderRight = "1px solid #d3d3d3")
#'           ),
#'           ## Points ----
#'           ### PF ----
#'           PFG = colDef(
#'             name = "PF",
#'             format = colFormat(digits = 2)
#'           ),
#'           ### PA ----
#'           PAG = colDef(
#'             name = "PA",
#'             format = colFormat(digits = 2)
#'           ),
#'           ### MOV ----
#'           MOV = colDef(
#'             format = colFormat(digits = 2),
#'             style = list(borderRight = "1px solid #d3d3d3")
#'           ),
#'           ## Simple Rating System ----
#'           ### SOS ----
#'           SOS = colDef(
#'             format = colFormat(digits = 2)
#'           ),
#'           ### SRS ----
#'           SRS = colDef(
#'             format = colFormat(digits = 2),
#'             style = color_scales(
#'               data = overviewData(),
#'               colors = c("red","pink", "whitesmoke", "palegreen", "green"),
#'               bias = 1
#'             )
#'           ),
#'           ### OSRS ----
#'           OSRS = colDef(
#'             format = colFormat(digits = 2)
#'           ),
#'           ### DSRS ----
#'           DSRS = colDef(
#'             format = colFormat(digits = 2)
#'           )
#'         )
#'       )
#'       overviewDataReact
#'     })
#'   })
#' }
#'
#'
#' #' @title Team Rankings Overview Table (UI)
#' #' @description Output UI for a team_rankings table module.
#' #' @param id Shiny module id.
#' #' @return A UI output for reactable standings table.
#' #' @export
#' #' @noRd
#' mod_team_rankings_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     fluidRow(
#'       div(style = "margin-right: 1rem",
#'           virtualSelectInput(
#'             inputId = ns("season"),
#'             label = "Select season",
#'             choices = seq(2007, get_current_season()),
#'             selected = get_current_season()
#'           )
#'       ),
#'       br(),
#'       fluidRow(
#'         tabBox(
#'           id = "team_rankings_tabBox",
#'           type = "pills",
#'           width = 12,
#'           #### Overview ----
#'           tabPanel(
#'             title = "Overview",
#'             value = "overview_tab"
#'             #teamRankingsOverviewUI("team_rank_overview")
#'           ),
#'           #### EPA ----
#'           tabPanel(
#'             title = "EPA",
#'             value = "epa_tab"
#'           ),
#'           #### ELO ----
#'           tabPanel(
#'             title = "Elo",
#'             value = "elo_tab"
#'           ),
#'           #### SRS ----
#'           tabPanel(
#'             title = "SRS",
#'             value = "srs_tab"
#'           )
#'         ) # end Team Rankings Tab box
#'       )
#'     )
#'   )
#' }
#'
#'
#' mod_team_rankings_server <- function(id,
#'                                      season_week_data,
#'                                      data,
#'                                      team_data){
#'   moduleServer(id, function(input, output, session){
#'     season <- reactive(input$season)
#'
#'     rankings_data <- reactive({
#'       df <- load_model_data_long(seasons = season())
#'     })
#'   }
#'
#'
#'
#'
#'
#'
