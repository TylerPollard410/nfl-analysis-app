# tests/testthat/test-fct-load-model-data.R

test_that("load_model_data calls get_season_dataset and returns data.frame", {
  seasons_input <- c(2018, 2019)
  fake_df <- data.frame(
    season      = c(2018, 2019),
    feature_val = c(0.5, 1.2),
    stringsAsFactors = FALSE
  )

  pkg_env <- environment(load_model_data)

  local_mocked_bindings(
    validate_seasons   = function(seasons) TRUE,
    get_season_dataset = function(dataset, seasons) {
      expect_equal(dataset, "model_data")
      expect_equal(seasons, seasons_input)
      fake_df
    },
    .env = pkg_env
  )

  result <- load_model_data(seasons_input)
  expect_s3_class(result, "data.frame")
  expect_equal(result, fake_df)
})

test_that("load_model_data errors on invalid seasons", {
  pkg_env <- environment(load_model_data)

  local_mocked_bindings(
    validate_seasons = function(seasons) stop("invalid"),
    .env             = pkg_env
  )
  expect_error(load_model_data("invalid"), "invalid")
})
