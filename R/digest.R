library(tidyverse)
library(glue)
library(lubridate)
library(assertr)
library(gmailr)

# Load custom functions
source("R/functions.R")

# Get current day and month in UTC
current_day <- today("UTC")
current_month <- floor_date(current_day, "month")

# Filter to proposals to include in email, add status and text to print
proposals <-
  fetch_issues("pteridogroup/ppg") |>
  filter(state == "open") |>
  # Only include taxonomic proposals (should have an entry for rank)
  filter(!is.na(rank)) |>
  mutate(
    created_at = ymd_hms(created_at, tz = "UTC"),
    created_month = floor_date(created_at, "month"),
    status = case_when(
      created_month < current_month %m-% months(2) ~ "Older",
      str_detect(title, "POSTPONED") ~ "Postponed",
      created_month == current_month ~ "Submitted this month",
      created_month == current_month %m-% months(1) ~
        "Submitted last month (to go in next ballot)",
      created_month == current_month %m-% months(2) ~
        "On current ballot"
    )
    ) |>
  assert(not_na, status) |>
  filter(status != "Older") |>
  arrange(desc(created_at)) |>
  mutate(
    text = glue::glue(
      "{number}. {title}: {url} ({year(created_at)}-{month(created_at)}-{day(created_at)})" # nolint
    )
  )

# Split out postponed issues
postponed_issues <- proposal_df2txt(proposals, "Postponed", ret_empty = "null")
postponed_header <- if (!is.null(postponed_issues)) {
  "<p>Postponed</p>"
} else {
  NULL
}

# Generate email text
digest_subject <- glue::glue("PPG Email Digest {current_day}")
digest_body <- c(
  glue::glue("<p><b>{digest_subject}</b></p>"),
  paste(
    "<p>This is an automated email summarizing taxonomic proposals submitted",
    "to PPG (https://github.com/pteridogroup/ppg). It is sent to everyone",
    "on the PPG mailing list once per week. If you do not wish to receive",
    "it or have any questions, please contact Eric Schuettpelz",
    "(schuettpelze@si.edu) or Joel Nitta (joelnitta@gmail.com).</p>",
    "<p>Only proposals that have not yet been decided are shown.",
    "Note that numbering of proposals is unique, but not consecutive",
    "(some numbers may be skipped).</p>",
    "<p>It is also recommended to subscribe to GitHub notifications for PPG.",
    "For instructions, see the",
    "<a href = 'https://pteridogroup.github.io/github.html#notifications'>GitHub guide</a>.</p>"), # nolint
  "<p><b>DO NOT REPLY TO THIS EMAIL</b>; no response will be given.</p>",
  "<hr>",
  "<p>Proposals submitted this month</p>",
  proposal_df2txt(proposals, "this month"),
  "<p>Proposals submitted last month (will go on next ballot)</p>",
  proposal_df2txt(proposals, "last month"),
  "<p>Proposals submitted two months ago (on current ballot)</p>",
  proposal_df2txt(proposals, "current ballot"),
  postponed_header,
  postponed_issues
) |>
  paste(collapse = "")

digest_email <-
  gm_mime() |>
  gm_to("ourPPG@googlegroups.com") |>
  gm_from("pteridogroup.no.reply@gmail.com") |>
  gm_subject(digest_subject) |>
  gm_html_body(digest_body)

# Authenticate email server
options(gargle_oauth_cache = ".secrets")
secret_json <- list.files(
  ".secrets", pattern = "client_secret.*json", full.names = TRUE)
gm_auth_configure(path = secret_json)
gm_oauth_client()
gm_auth("pteridogroup.no.reply@gmail.com")

# Verify it looks correct, i.e. look at your Gmail drafts in the browser
# gm_create_draft(digest_email) # nolint

# or just send the existing MIME message
gm_send_message(digest_email)
