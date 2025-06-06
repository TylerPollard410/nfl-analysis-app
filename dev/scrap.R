data2023 <- load_season_standings(2023, base_url = "https://github.com/TylerPollard410/nfl-analysis-app/tree/main/data/season_standings_data")
https://github.com/TylerPollard410/nfl-analysis-app/tree/main/data/season_standings_data/2023

data2023 <- readRDS(file = url("https://github.com/TylerPollard410/nfl-analysis-app/tree/main/data/season_standings_data/2023/season_standings_data_2023.rds"))
data/season_standings_data/2023/season_standings_data_2023.rds
https://github.com/TylerPollard410/nfl-analysis-app/blob/c1c41a8fd10d16e888e11ca8111d48f31de3d7c3/data/season_standings_data/2023/season_standings_data_2023.rds


standings_2023 <- readRDS(url("https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/2023/season_standings_data_2023.rds"))
str(standings_2023)

standings_2023 <- readRDS(url("https://github.com/TylerPollard410/nfl-analysis-app/raw/main/data/season_standings_data/2023/season_standings_data_2023.rds"))
str(standings_2023)

standings_2023_local <- readRDS("data/season_standings_data/2023/season_standings_data_2023.rds")
standings_2023_remote <- readRDS(url(
  "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/2023/season_standings_data_2023.rds",
  open = "rb"
))
str(standings_2023)

system.time(
  download.file(
    "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/2023/season_standings_data_2023.rds",
    destfile = "tmp_standings_2023.rds",
    mode = "wb"
  )
)
system.time(
  test <- readRDS("tmp_standings_2023.rds")
)
str(test)


url_temp <- url("https://github.com/TylerPollard410/nfl-analysis-app/raw/main/data/season_standings_data/2023/season_standings_data_2023.rds")
gz_url <- gzcon(url_temp)
standings_2023 <- nflreadr::rds_from_url("https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/2023/season_standings_data_2023.rds")

standings_2023 <- readRDS(url_temp)



url2 <- "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/2023/season_standings_data_2023.rds"
url_temp2 <- url(url2, open = "rb")
gz_url2 <- gzcon(url_temp)
standings_2023 <- nflreadr::rds_from_url("https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/2023/season_standings_data_2023.rds")
standings_2023 <- readRDS(url_temp2)

url_temp3 <- url("https://github.com/TylerPollard410/NFL-Analysis-Test/raw/refs/heads/main/app/data/playerOffenseData.rda")
dat3 <- readRDS(url_temp3)

url <- "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_data/2023/season_standings_data_2023.rds"
data <- readRDS(url(url))







