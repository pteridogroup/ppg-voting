# Count the number of commenters on issues

library(tidyverse)
library(httr)
library(gh)
library(gitcreds)
library(oskeyring)
source("R/functions.R")

# This requires github authentication.
# See <https://github.com/r-lib/gh#authentication>

# Get dataframe of all isssues
issues <- fetch_issues("pteridogroup/ppg")

# Extract issue numbers
issue_nums <- issues$number

# Make tibble of commenters
commenters <- purrr::map_df(issue_nums, fetch_commenters)

# Number of unique commenters
commenters |>
  filter(!is.na(commenter)) |>
  count(commenter)

# Number of unique users who have created issues
issues |>
  count(user)