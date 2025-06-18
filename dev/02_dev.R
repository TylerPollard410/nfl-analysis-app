# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
usethis::use_package("bs4Dash", type = "Imports")
usethis::use_package("cachem")
usethis::use_package("fresh")
#usethis::use_dev_package("nflverse")
usethis::use_package("memoise")
usethis::use_dev_package("nflfastR", remote = "nflverse/nflfastR")
usethis::use_dev_package("nflseedR", remote = "nflverse/nflseedR")
usethis::use_dev_package("nflplotR", remote = "nflverse/nflplotR")
usethis::use_dev_package("nflreadr", remote = "nflverse/nflreadr")
usethis::use_package("reactable")
usethis::use_package("reactablefmtr")
usethis::use_package("shiny")
usethis::use_package("shinycssloaders")
usethis::use_package("shinyjs")
usethis::use_package("shinyWidgets")
# tidyverse package
usethis::use_package("dplyr")
usethis::use_package("stringr")
#usethis::use_package("tidyverse", type = "Depends")
usethis::use_package("waiter")

## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "standings", with_test = TRUE) # Name of the module
golem::add_module(name = "team_rankings", with_test = TRUE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("load_elo_data", with_test = TRUE)
golem::add_fct("load_epa_data", with_test = TRUE)
golem::add_fct("load_model_data", with_test = TRUE)
golem::add_fct("load_model_data_long", with_test = TRUE)
golem::add_fct("load_redzone_data", with_test = TRUE)
golem::add_fct("load_scores_data", with_test = TRUE)
golem::add_fct("load_season_standings_data", with_test = TRUE)
golem::add_fct("load_series_data", with_test = TRUE)
golem::add_fct("load_srs_data", with_test = TRUE)
golem::add_fct("load_turnover_data", with_test = TRUE)
golem::add_fct("load_weekly_standings_data", with_test = TRUE)

golem::add_utils("data_url", with_test = FALSE)
golem::ad

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")
golem::add_any_file("file.json")


## Add internal datasets ----
## If you have data in your package
#usethis::use_data_raw(name = "my_dataset", open = FALSE)
usethis::use_data_raw()

## Tests ----
## Add one line by test you want to create
usethis::use_test("fct_get_season_data")

# Documentation

## Vignette ----
usethis::use_vignette("nflanalysisapp")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actionsssh -T git@github.com
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
