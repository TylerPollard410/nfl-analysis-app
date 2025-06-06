# —– STARTUP (runs once per app process) —–
all_seasons     <- 2006:nflreadr::most_recent_season()
teamsData       <- nflreadr::load_teams(current = FALSE)
game_data       <- load_game_data(seasons = all_seasons)
game_data_long  <- load_game_data_long(game_df = game_data)
# —– END STARTUP —–
