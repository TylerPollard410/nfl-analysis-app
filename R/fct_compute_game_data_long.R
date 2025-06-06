#' Compute Long-Format Team-Game Data from Game Schedule
#'
#' Takes a game-level data frame and generates a long-format team-game data set with per-team rolling records and game stats.
#'
#' @param game_df Data frame of game schedule data. Should contain columns for teams, opponent, result, spread_line, team_score, opponent_score, season, game_id, winner, and location.
#'
#' @return A tibble in long format with team-game rows, including per-team cumulative statistics (games played, wins, losses, ties, points for/against, per-game averages) and derived flags (winner, locationID).
#'
#' @details
#' The function reshapes the data to long format, computes running tallies for team record and scoring stats, and annotates winner flags for each game. Relies on \code{clean_homeaway()} to handle home/away structure.
#'
#' @seealso \code{\link{clean_homeaway}}
#'
#' @export
#' @noRd
compute_game_data_long <- function(game_df = game_data) {
  gameDataLong <- game_df |>
    clean_homeaway(invert = c("result", "spread_line")) |>
    group_by(season, team) |>
    mutate(
      team_GP = row_number(),
      winner = ifelse(team == winner, TRUE,
                      ifelse(opponent == winner, FALSE, NA)),
      team_W = cumsum(result > 0),
      team_L = cumsum(result < 0),
      team_T = team_GP - team_W - team_L,
      team_PF = cumsum(team_score),
      team_PFG = team_PF/team_GP,
      team_PA = cumsum(opponent_score),
      team_PAG = team_PA/team_GP,
    ) |>
    mutate(
      team_W = ifelse(is.na(lag(team_W)), 0, lag(team_W)),
      team_L = ifelse(is.na(lag(team_L)), 0, lag(team_L)),
      team_T = ifelse(is.na(lag(team_T)), 0, lag(team_T))
    ) |>
    ungroup() |>
    group_by(game_id) |>
    mutate(locationID = row_number(), .after = location) |>
    ungroup()

  return(gameDataLong)
}
