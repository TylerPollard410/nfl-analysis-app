# tests/testthat/test-fct-load-model-data-long.R

test_that("load_model_data_long uses provided model_data and prunes correctly", {
  # Create a fake wide model_data with various opponent_ columns
  wide_fake <- data.frame(
    season               = 2021,
    game_id              = 10,
    team                 = "A",
    opponent             = "B",
    result               = 7,
    spread_line          = 3,
    team_score           = 28,
    opponent_score       = 21,
    location             = "home",
    winner               = "A",
    opponent_rest        = 1,
    opponent_moneyline   = -120,
    opponent_spread_odds = 105,
    opponent_extra_col   = "REMOVE",
    stringsAsFactors     = FALSE
  )
  # Simulate clean_homeaway returning the same data plus a marker
  fake_long <- wide_fake
  fake_long$marker <- TRUE

  pkg_env <- environment(load_model_data_long)

  local_mocked_bindings(
    clean_homeaway = function(df, invert) {
      expect_identical(df, wide_fake)
      expect_equal(invert, c("result", "spread_line"))
      fake_long
    },
    .env = pkg_env
  )

  # Call with model_data provided and prune_opponent = TRUE
  result <- load_model_data_long(model_data = wide_fake, prune_opponent = TRUE)
  expect_s3_class(result, "data.frame")

  # Allowed opponent_ columns remain
  expect_true("opponent_score" %in% names(result))
  expect_true("opponent_rest" %in% names(result))
  expect_true("opponent_moneyline" %in% names(result))
  expect_true("opponent_spread_odds" %in% names(result))

  # Disallowed column removed
  expect_false("opponent_extra_col" %in% names(result))
  # Marker preserved
  expect_true("marker" %in% names(result))
})

test_that("load_model_data_long respects prune_opponent = FALSE", {
  wide_fake <- data.frame(
    season             = 2022,
    game_id            = 5,
    team               = "P",
    opponent           = "Q",
    result             = 2,
    spread_line        = 1,
    team_score         = 14,
    opponent_score     = 12,
    location           = "away",
    winner             = "P",
    opponent_extra_col = "KEEP",
    stringsAsFactors   = FALSE
  )
  fake_long <- wide_fake
  fake_long$marker <- TRUE

  pkg_env <- environment(load_model_data_long)

  local_mocked_bindings(
    clean_homeaway = function(df, invert) {
      expect_identical(df, wide_fake)
      fake_long
    },
    .env = pkg_env
  )

  # prune_opponent = FALSE should keep opponent_extra_col
  result <- load_model_data_long(model_data = wide_fake, prune_opponent = FALSE)
  expect_true("opponent_extra_col" %in% names(result))
})

test_that("load_model_data_long loads from seasons when model_data is NULL", {
  seasons_input <- c(2020, 2021)
  # Fake wide data for two seasons
  wide_2020 <- data.frame(
    season         = 2020,
    game_id        = 1,
    team           = "X",
    opponent       = "Y",
    result         = 10,
    spread_line    = 5,
    team_score     = 30,
    opponent_score = 20,
    location       = "away",
    winner         = "X",
    stringsAsFactors = FALSE
  )
  wide_2021 <- data.frame(
    season         = 2021,
    game_id        = 2,
    team           = "Z",
    opponent       = "W",
    result         = -3,
    spread_line    = -2,
    team_score     = 17,
    opponent_score = 20,
    location       = "home",
    winner         = "W",
    stringsAsFactors = FALSE
  )
  combined_wide <- rbind(wide_2020, wide_2021)
  # Simulate clean_homeaway returning combined_wide with an extra pruneable column and a marker
  fake_long <- combined_wide
  fake_long$opponent_extra <- "REMOVE"
  fake_long$marker <- TRUE

  pkg_env <- environment(load_model_data_long)

  local_mocked_bindings(
    validate_seasons = function(seasons) TRUE,
    load_model_data = function(seasons) {
      expect_equal(seasons, seasons_input)
      combined_wide
    },
    clean_homeaway = function(df, invert) {
      expect_identical(df, combined_wide)
      expect_equal(invert, c("result", "spread_line"))
      fake_long
    },
    .env = pkg_env
  )

  result <- load_model_data_long(model_data = NULL,
                                 seasons = seasons_input,
                                 prune_opponent = TRUE)
  expect_s3_class(result, "data.frame")
  # Pruned opponent_extra
  expect_false("opponent_extra" %in% names(result))
  expect_true("marker" %in% names(result))
})

test_that("load_model_data_long errors on invalid seasons when model_data is NULL", {
  pkg_env <- environment(load_model_data_long)

  local_mocked_bindings(
    validate_seasons = function(seasons) stop("invalid"),
    .env = pkg_env
  )
  expect_error(load_model_data_long(model_data = NULL, seasons = "invalid"), "invalid")
})
