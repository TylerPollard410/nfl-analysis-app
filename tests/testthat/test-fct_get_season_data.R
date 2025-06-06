# tests/testthat/test-get-season-dataset.R


test_that("get_season_dataset binds multiple seasons and uses cache", {
  # Counter to track how many times download_and_read_rds is called
  counter <- 0

  # Create mock functions without relying on tibble
  fake_download <- function(url) {
    counter <<- counter + 1
    # Return a data.frame whose 'season' column matches the URL string parsed as a number
    season_val <- as.numeric(url)
    data.frame(season = season_val, value = paste0("data_", season_val), stringsAsFactors = FALSE)
  }

  fake_build_url <- function(dataset, season) {
    # Return the season as a string so fake_download can parse it
    as.character(season)
  }

  # Determine the environment where get_season_dataset is defined
  pkg_env <- environment(get_season_dataset)

  # Use local_mocked_bindings to temporarily override functions
  local_mocked_bindings(
    download_and_read_rds = fake_download,
    build_data_url       = fake_build_url,
    .env                 = pkg_env
  )

  # First call: should invoke fake_download twice (for two seasons)
  df <- get_season_dataset("dummy_dataset", c(2001, 2002))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)                     # one row per season
  expect_equal(df$season, c(2001, 2002))
  expect_equal(df$value, c("data_2001", "data_2002"))
  expect_equal(counter, 2L)

  # Second call with same arguments: should use cache, so counter stays 2
  df2 <- get_season_dataset("dummy_dataset", c(2001, 2002))
  expect_identical(df2, df)
  expect_equal(counter, 2L)

  # Call with a new season added: should download only for the new season
  df3 <- get_season_dataset("dummy_dataset", c(2001, 2002, 2003))
  expect_equal(counter, 3L)
  expect_equal(df3$season, c(2001, 2002, 2003))
  expect_equal(df3$value, c("data_2001", "data_2002", "data_2003"))
})
