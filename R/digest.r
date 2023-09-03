library(tidyverse)
library(glue)
library(lubridate)
library(assertr)

# Load custom functions
source("R/functions.R")

# Get current day in UTC
current_day <- today("UTC")

# Filter to proposals to include in email, add status and text to print
proposals <-
  fetch_issues("pteridogroup/ppg")|>
  filter(state == "open") |>
  mutate(created_at = ymd_hms(created_at, tz = "UTC")) |>
  mutate(
    status = case_when(
      year(created_at) < year(current_day) - 2 ~ "Older",
      month(created_at) < month(current_day) - 2 ~ "Older",
      month(created_at) == month(current_day) - 2 ~ "On current ballot",
      month(created_at) == month(current_day) - 1 ~
        "Submitted last month (to go in next ballot)",
      month(created_at) == month(current_day) ~ "Submitted this month"
    )
  ) |>
  assert(not_na, status) |>
  filter(status != "Older") |>
  arrange(desc(created_at)) |>
  mutate(
    text = glue::glue(
      "{number}. {title}: {url} ({year(created_at)}-{month(created_at)}-{day(created_at)})" # nolint
    )
  ) |>
  arrange(proposals, desc(created_at))

# Generate email text
c(
  glue::glue("PPG Email Digest {current_day}\n\n"),
  paste("This is an automated email summarizing taxonomic proposals submitted",
    "to PPG (https://github.com/pteridogroup/ppg). It is sent to everyone",
    "on the PPG mailing list once per week. If you do not wish to receive",
    "it or have any questions, please contact Eric Schuettpelz",
    "(schuettpelze@si.edu).\n\nOnly",
    "proposals that have not yet been decided are shown.",
    "Note that numbering of proposals is unique, but not consecutive",
    "(some numbers may be skipped)."),
  "\nDO NOT REPLY TO THIS EMAIL; no response will be given.\n",
  "------\n",
  "Proposals submitted this month\n",
  proposal_df2txt(proposals, "this month"),
  "\nProposals submitted last month (will go on next ballot)\n",
  proposal_df2txt(proposals, "last month"),
  "\nProposals submitted two months ago (on current ballot)\n",
  proposal_df2txt(proposals, "current ballot")
) |>
  write_lines(
    "results/digest_email.txt"
  )
