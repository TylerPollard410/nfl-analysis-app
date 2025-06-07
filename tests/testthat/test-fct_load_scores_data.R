# tests/testthat/test-fct-load-scores-data.R

test_that("load_scores_data calls get_season_dataset and returns data.frame", {
  seasons_input <- c(2018, 2019)
  fake_df <- data.frame(
    season = c(2018, 2019),
    score  = c(24, 31),
    stringsAsFactors = FALSE
  )

  pkg_env <- environment(load_scores_data)

  local_mocked_bindings(
    validate_seasons    = function(seasons) TRUE,
    get_season_dataset  = function(dataset, seasons) {
      expect_equal(dataset, "scores_data")
      expect_equal(seasons, seasons_input)
      fake_df
    },
    .env = pkg_env
  )

  result <- load_scores_data(seasons_input)
  expect_s3_class(result, "data.frame")
  expect_equal(result, fake_df)
})

test_that("load_scores_data errors on invalid seasons", {
  pkg_env <- environment(load_scores_data)

  local_mocked_bindings(
    validate_seasons = function(seasons) stop("invalid"),
    .env             = pkg_env
  )
  expect_error(load_scores_data("invalid"), "invalid")
})
