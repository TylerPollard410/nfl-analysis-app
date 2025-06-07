# tests/testthat/test-fct-load-weekly-standings-data.R

test_that("load_weekly_standings_data calls get_season_dataset and returns data.frame", {
  seasons_input <- c(2018, 2019)
  fake_df <- data.frame(
    season       = c(2018, 2019),
    week         = c(1, 2),
    wins         = c(10, 11),
    losses       = c(6, 5),
    stringsAsFactors = FALSE
  )

  pkg_env <- environment(load_weekly_standings_data)

  local_mocked_bindings(
    validate_seasons   = function(seasons) TRUE,
    get_season_dataset = function(dataset, seasons) {
      expect_equal(dataset, "weekly_standings_data")
      expect_equal(seasons, seasons_input)
      fake_df
    },
    .env = pkg_env
  )

  result <- load_weekly_standings_data(seasons_input)
  expect_s3_class(result, "data.frame")
  expect_equal(result, fake_df)
})

test_that("load_weekly_standings_data errors on invalid seasons", {
  pkg_env <- environment(load_weekly_standings_data)

  local_mocked_bindings(
    validate_seasons = function(seasons) stop("invalid"),
    .env             = pkg_env
  )
  expect_error(load_weekly_standings_data("invalid"), "invalid")
})
