test_that("load_season_standings returns a data frame for a valid season", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()

  # Replace 2023 with a season you know exists in your GitHub data
  out <- load_season_standings(2023)
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  # Optionally, check for expected columns (adjust as needed)
  expect_true(all(c("season", "team") %in% names(out)))
})

test_that("load_season_standings handles multiple seasons", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()

  out <- load_season_standings(c(2022, 2023))
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(length(unique(out$season)) >= 2)
})

test_that("load_season_standings errors with invalid seasons argument", {
  expect_error(load_season_standings("2023"), "Seasons argument must be a non-empty numeric vector")
  expect_error(load_season_standings(NA), "Seasons argument must be a non-empty numeric vector")
  expect_error(load_season_standings(NULL), "Seasons argument must be a non-empty numeric vector")
  expect_error(load_season_standings(numeric(0)), "Seasons argument must be a non-empty numeric vector")
})

test_that("build_github_raw_url constructs the correct URL", {
  url <- build_github_raw_url("season_standings_2023.rds")
  expect_equal(
    url,
    "https://raw.githubusercontent.com/TylerPollard410/nfl-analysis-app/main/data/season_standings_2023.rds"
  )
})

test_that("validate_seasons accepts good input, errors on bad input", {
  expect_invisible(validate_seasons(2023))
  expect_invisible(validate_seasons(c(2022, 2023)))
  expect_error(validate_seasons(NA))
  expect_error(validate_seasons("2023"))
  expect_error(validate_seasons(NULL))
  expect_error(validate_seasons(numeric(0)))
})
