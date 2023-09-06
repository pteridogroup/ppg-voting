library(tidyverse)
library(glue)
library(lubridate)
library(assertr)
library(gmailr)

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
  )

# Generate email text
digest_subject <- glue::glue("PPG Email Digest {current_day}")
digest_body <- c(
  glue::glue("<p><b>{digest_subject}</b></p>"),
  paste(
    "<p>This is an automated email summarizing taxonomic proposals submitted",
    "to PPG (https://github.com/pteridogroup/ppg). It is sent to everyone",
    "on the PPG mailing list once per week. If you do not wish to receive",
    "it or have any questions, please contact Eric Schuettpelz",
    "(schuettpelze@si.edu).</p>",
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
  proposal_df2txt(proposals, "current ballot")
) |>
  paste(collapse = "")

digest_email <-
  gm_mime() |>
  gm_to("joelnitta@gmail.com") |>
  gm_from("pteridogroup.no.reply@gmail.com") |>
  gm_subject(glue::glue("[{digest_subject}]")) |>
  gm_html_body(digest_body)

# Authenticate email server
options(gargle_oauth_cache = ".secrets")
secret_json <- list.files(
  ".secrets", pattern = "client_secret.*json", full.names = TRUE)
gm_auth_configure(path = secret_json)
gm_oauth_client()
gm_auth("pteridogroup.no.reply@gmail.com")

# or the existing MIME message
gm_send_message(digest_email)
