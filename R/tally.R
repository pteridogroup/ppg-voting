library(tidyverse)
library(glue)

# Load custom functions
source("R/functions.R")

# Set variables - change this each time ballot is tallied
ballot_number <- "3"
vote_period <- "September 2023"
ballot_cutoff <- "2023-09-30 23:59:50"
ballot_file <- "https://docs.google.com/spreadsheets/d/1oiSeReaWsSJn4AA17pRP9rRGdE4i_0Zo-kvokudlAXg/edit?usp=sharing" # nolint

# Check ballots
ballot_checked <- check_ballot(
  ballot_file,
  ballot_cutoff,
  # should be a URL to PPG2 email list on Google Sheets
  email_file = "https://docs.google.com/spreadsheets/d/1vxlmf8QPndiE6dIeDcjoFT7GA3ZE4pSc_Z1raf4iuwA/edit?usp=sharing" # nolint
)

# Inspect ballots that fail any checks:
# - ballots not passing email check (email not in list)
ballot_checked |>
  filter(email_check == FALSE)

# - ballots not passing time check (submitted after deadline)
ballot_checked |>
  filter(time_check == FALSE)

# - ballots not passing name check (same person submitted multiple times)
# This is actually OK since on the most recent vote will be counted.
ballot_checked |>
  filter(name_check == FALSE)

# Filter out votes that don't pass checks and tally
votes_tally <- tally_votes(ballot_checked)

# Write out tally table
votes_tally |>
  write_csv(glue("results/ballot-{ballot_number}_results.csv"))

# Format tally results for posting to GitHub
format_tally(votes_tally, ballot_number, vote_period) |>
  write_csv(glue("results/ballot-{ballot_number}_results_text.csv"))
