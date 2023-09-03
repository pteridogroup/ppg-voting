library(tidyverse)
library(glue)

# Load custom functions
source("R/functions.R")

# Set variables - change this each time ballot is tallied
ballot_number <- "2"
vote_period <- "August 2023"
ballot_cutoff <- "2023-08-31 23:59:50"
ballot_file <- "https://docs.google.com/spreadsheets/d/1HiJP3RmxTDVJy14KPx4wsn2MLgYwdrkYmCf2ac_tvMo/edit?usp=sharing" # nolint

# Check ballots
ballot_checked <- check_ballot(
  ballot_file,
  ballot_cutoff,
  # should be a URL to PPG2 email list on Google Sheets
  email_file = "https://docs.google.com/spreadsheets/d/1QxOP-7N0IKgeNA_gdh2bGmRZKGPgf__IRGYG_tn1dDI/edit#gid=0" # nolint
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
