library(tidyverse)
library(glue)
library(gmailr)

# Load custom functions
source("R/functions.R")

# Set variables - change this each time ballot is tallied
ballot_number <- "5"
vote_period <- "November 2023"
ballot_file <- "https://docs.google.com/spreadsheets/d/1mnKdEqd5wFlnIdWzlj9UEq6Gc3TgeeCM9YGpy_OCr54/edit?usp=sharing" # nolint

# Check ballots
ballot_checked <- check_ballot(
  ballot_file,
  ballot_cutoff,
  # should be a URL to PPG2 email list on Google Sheets
  email_file = "https://docs.google.com/spreadsheets/d/1vxlmf8QPndiE6dIeDcjoFT7GA3ZE4pSc_Z1raf4iuwA/edit?usp=sharing" # nolint
)

# Only for Ballot #4: load issue numbers
# TODO: next time, include issue number in Google Form output
issue_nums <- read_csv("data/ballot-4_issues.csv") %>%
  select(num, proposal)

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
# TODO: Add Issue number for easy lookup
# TODO: Automate commenting, renaming subject, and closing
format_tally_github(votes_tally, ballot_number, vote_period) |>
  write_csv(glue("results/ballot-{ballot_number}_results_text.csv"))

# Format email
vote_results_email_list <- format_tally_email(
  votes_tally, ballot_number, vote_period, issue_nums) 

vote_results_email <-
  gm_mime() %>%
  gm_to("ourPPG@googlegroups.com") %>%
  gm_from("ourPPG@googlegroups.com") %>%
  gm_subject(glue("PPG Ballot {ballot_number} Results")) %>%
  gm_html_body(
    glue(
      "<p><b>PPG Ballot {ballot_number} Results (Voting Period {vote_period})</b></p>",
      "<p>This is an automated email summarizing the results of voting on \\
      taxonomic proposals submitted to PPG \\
      (https://github.com/pteridogroup/ppg). It is sent to everyone on \\
      the PPG mailing list once per month. If you do not wish to receive it \\
      or have any questions, please contact Eric Schuettpelz \\
      (schuettpelze@si.edu) or Joel Nitta (joelnitta@gmail.com).</p>",
      "<p>Please note the following:</p> \\
      <ul> \\
      <li>Numbering of proposals is unique, but not consecutive (some \\
      numbers may be skipped). \\
      <li>A 2/3 majority is required to pass \\
      </ul>",
      "<p>DO NOT REPLY TO THIS EMAIL</br>; no response will be given.</p><hr>",
      "<ul>{vote_results_email_list}</ul>",
      "<p>Thank you very much for your participation in PPG!</p>"
      )
  )

# Authenticate email server
options(gargle_oauth_cache = ".secrets")
secret_json <- list.files(
  ".secrets", pattern = "client_secret.*json", full.names = TRUE)
gm_auth_configure(path = secret_json)
gm_oauth_client()
gm_auth("pteridogroup.no.reply@gmail.com")

# Verify it looks correct, i.e. look at your Gmail drafts in the browser
gm_create_draft(vote_results_email) # nolint

# or just send the existing MIME message
# gm_send_message(digest_email)
