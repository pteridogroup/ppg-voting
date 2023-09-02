library(tidyverse)
library(lubridate)
library(janitor)
library(assertr)
library(googlesheets4)

# Load and format ballot ----

ballot_file <- "data/ppg_ballot-2.csv"
ballot_cutoff <- "2023-08-31 23:59:50"

ballot <- read_csv(ballot_file, show_col_types = FALSE) |>
  rename(timestamp = Timestamp, email = Username) |>
  mutate(email = tolower(email)) |>
  # Convert time stamp to Greenwich time (same as UTC)
  mutate(timestamp = str_remove(timestamp, " GMT\\+9")) |>
  mutate(timestamp = ymd_hms(timestamp, tz = "Greenwich") + hours(9)) |>
  # Add voting cutoff
  mutate(cutoff = ymd_hms(ballot_cutoff, tz = "Greenwich"))

# Load and format email list -----

# PPG mailing-list email is in column "email" and other emails are in column
# "other email". Combine this into single email column.
ppg_emails <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1vxlmf8QPndiE6dIeDcjoFT7GA3ZE4pSc_Z1raf4iuwA/edit#gid=0" # nolint
  ) |>
  clean_names()

# add 'other email' so that each name may have multiple email addresses
ppg_emails <-
  ppg_emails |>
  select(name, email = primary) |>
  bind_rows(
    select(ppg_emails, name, email = other_email)
  ) |>
  filter(!is.na(email)) |>
  mutate(email = tolower(email)) |>
  unique() |>
  assert(is_uniq, email) |>
  assert(not_na, name)

# Conduct checks
ballot_checked <-
  ballot |>
  # Add check that all emails are in PPG list
  left_join(select(ppg_emails, name = name, email = email), by = "email") |>
  mutate(email_check = !is.na(name)) |>
  # Add check that all emails pass time stamp cutoff
  mutate(time_check = timestamp < cutoff) |>
  # Add check for duplicated names of submitters
  # only keep most recent vote per person
  arrange(name, desc(timestamp)) |>
  mutate(name_check = !duplicated(name))

ballot_checked |>
  filter(email_check == FALSE)

ballot_checked |>
  filter(time_check == FALSE)

ballot_checked |>
  filter(name_check == FALSE)

# Filter out votes that don't pass checks
ballot_final <-
  ballot_checked |>
  filter(email_check == TRUE) |>
  filter(time_check == TRUE) |>
  filter(name_check == TRUE) |>
  select(-contains("check"))

# Tally votes
tally_final <-
  ballot_final |>
  select(-timestamp, -email, -`GitHub username`, -cutoff, -name) |>
  pivot_longer(names_to = "proposal", values_to = "response", everything()) |>
  group_by(proposal) |>
  count(response) |>
  filter(response != "Abstain") |>
  mutate(total = sum(n)) |>
  ungroup() |>
  rowwise() |>
  mutate(percent = (n / total) * 100)

tally_n <-
  tally_final |>
  pivot_wider(names_from = response, values_from = n, id_cols = proposal) |>
  mutate(No = replace_na(No, 0)) |>
  rename(no_n = No, yes_n = Yes)

tally_p <-
  tally_final |>
  mutate(percent = round(percent, 1)) |>
  pivot_wider(
    names_from = response, values_from = percent, id_cols = proposal) |>
  mutate(No = replace_na(No, 0)) |>
  rename(no_p = No, yes_p = Yes)

left_join(tally_n, tally_p, by = "proposal") |>
  rowwise() |>
  mutate(total = sum(no_n, yes_n, na.rm = TRUE)) |>
  mutate(
    min_to_pass = round((2 / 3) * total, 0),
    result = if_else(yes_n > min_to_pass, "passes", "does not pass")
  ) |>
  mutate(text = glue::glue(
    "This proposal was voted on during PPG Ballot 1 (voting period July 2023). A total of {total} votes were cast. There were {yes_n} 'Yes' votes ({yes_p}%) and {no_n} 'No' votes ({no_p}%). The proposal {result}.")) |>
  select(proposal, text)
