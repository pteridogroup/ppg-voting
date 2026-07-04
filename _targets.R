library(targets)

tar_option_set(
  packages = c(
    "tidyverse", "glue", "lubridate", "gmailr", "googlesheets4",
    "assertr", "janitor", "yaml", "httr2", "gargle", "jsonlite"
  )
)

source("R/functions.R")
source("R/state.R")
source("R/google_forms.R")
source("R/draft_ppg_ballot_email.R")
source("R/draft_ppg_results_email.R")

state_path <- "ballot_state.yaml"
ppg_email_list_url <- "https://docs.google.com/spreadsheets/d/1vxlmf8QPndiE6dIeDcjoFT7GA3ZE4pSc_Z1raf4iuwA/edit?usp=sharing" # nolint

list(
  # --- Shared state and inputs ---
  tar_target(state_file, state_path, format = "file"),
  tar_target(ballot_state, read_ballot_state(state_file)),
  tar_target(
    issues, fetch_issues("pteridogroup/ppg"),
    cue = tar_cue(mode = "always")
  ),

  # --- Ballot creation ---
  # Ballot number/period always comes from ballot_state.yaml (last completed
  # + 1 month), never from today's date, so a busy month can't silently
  # vanish. If a period has no eligible proposals, the ballot is skipped
  # (no empty Google Form) and state still advances so the next run moves
  # on to the following month.
  tar_target(next_creation, compute_next_creation(ballot_state)),
  tar_target(issues_to_vote, filter_issues_for_ballot(issues, next_creation)),
  tar_target(
    ballot_creation_result,
    create_ballot_form(next_creation, issues_to_vote, state_file)
  ),

  # Drafts the announcement email for any real ballot that has a form but
  # hasn't been announced yet. Kept independent of ballot_creation_result
  # above so it also catches a ballot whose announcement was missed or
  # failed in an earlier run (it reads ballot_state fresh, so this only
  # sees a ballot created in this same run on the *next* tar_make() call).
  tar_target(
    ballot_announcement_result,
    announce_pending_ballots(ballot_state, state_file)
  ),

  # Closes every real ballot whose voting period has ended but is still
  # accepting responses (via forms.setPublishSettings). Independent of
  # tallying below - closing just stops new submissions.
  tar_target(
    ballot_closing_result,
    close_expired_ballots(ballot_state, state_file)
  ),

  # --- Ballot tallying ---
  # Tallies whichever real ballot creation has already produced that
  # hasn't been tallied yet (see find_next_tally() in R/state.R) instead
  # of assuming a fixed monthly cadence; fetches responses directly from
  # the Forms API using the form_id recorded at creation time (no more
  # manual spreadsheet URL per ballot).
  tar_target(ppg_emails, load_ppg_emails(ppg_email_list_url)),
  tar_target(
    ballot_tally_result,
    run_ballot_tally(ballot_state, ppg_emails, state_file)
  ),

  # Reminder message on every run: when does the currently open ballot
  # close, in the local timezone as well as UTC. Useful for manually
  # setting the native Google Forms closing timer since that's not
  # exposed via the API.
  tar_target(
    ballot_deadline_reminder,
    report_current_ballot_deadline(ballot_state),
    cue = tar_cue(mode = "always")
  )
)
