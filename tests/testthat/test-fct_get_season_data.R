# tests/testthat/test-fct_get_season_dataset.R

test_that("get_season_dataset binds multiple seasons and uses cache", {
  # Ensure the cache is clear before starting (test isolation)
  clear_app_cache()

  # Track how many times download_and_read_rds is called (simulates remote I/O)
  counter <- 0

  # Mock download: returns a data.frame with season and value columns
  fake_download <- function(url) {
    counter <<- counter + 1
    season_val <- as.numeric(url)
    data.frame(
      season = season_val,
      value = paste0("data_", season_val),
      stringsAsFactors = FALSE
    )
  }

  # Mock URL builder: just returns season as string
  fake_build_url <- function(dataset, season) as.character(season)

  # Use the package environment for mocking (where fetch_one_season looks up its dependencies)
  pkg_env <- environment(get_season_dataset)

  # Temporarily replace data fetching functions with mocks
  local_mocked_bindings(
    download_and_read_rds = fake_download,
    build_data_url        = fake_build_url,
    .env                  = pkg_env
  )

  # First call: both seasons are downloaded (counter increments twice)
  df <- get_season_dataset("dummy_dataset", c(2001, 2002))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_equal(df$season, c(2001, 2002))
  expect_equal(df$value, c("data_2001", "data_2002"))
  expect_equal(counter, 2L)

  # Second identical call: uses cache, no new downloads (counter unchanged)
  df2 <- get_season_dataset("dummy_dataset", c(2001, 2002))
  expect_identical(df2, df)
  expect_equal(counter, 2L)

  # Add a new season: only that season is downloaded (counter increments once)
  df3 <- get_season_dataset("dummy_dataset", c(2001, 2002, 2003))
  expect_equal(counter, 3L)
  expect_equal(df3$season, c(2001, 2002, 2003))
  expect_equal(df3$value, c("data_2001", "data_2002", "data_2003"))
})
