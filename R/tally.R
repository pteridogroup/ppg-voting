library(tidyverse)
library(glue)
library(gmailr)

# Load custom functions
source("R/functions.R")
source("R/draft_ppg_results_email.R")

# Set variables - change this each time ballot is tallied
ballot_number <- "12"
vote_period <- "July 2024"
ballot_file <- "https://docs.google.com/spreadsheets/d/1GQ2oaAtrFQvSzU9s6RsUp73Iodgqks3yjOizdO5G4lw/edit?usp=sharing" # nolint

# Check ballots
ballot_checked <- check_ballot(
  ballot_file,
  # should be a URL to PPG2 email list on Google Sheets
  email_file = "https://docs.google.com/spreadsheets/d/1vxlmf8QPndiE6dIeDcjoFT7GA3ZE4pSc_Z1raf4iuwA/edit?usp=sharing" # nolint
)

# Inspect ballots that fail any checks:
# - ballots not passing email check (email not in list)
bad_emails <-
  ballot_checked |>
  filter(email_check == FALSE)

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
# TODO: Automate commenting, renaming subject, and closing
format_tally_github(votes_tally, ballot_number, vote_period) |>
  write_csv(glue("results/ballot-{ballot_number}_results_text.csv"))

# Draft email
draft_ppg_results_email(votes_tally, ballot_number, vote_period)

# Open pteridogroup.no.reply@gmail.com account, check drafts, and send
