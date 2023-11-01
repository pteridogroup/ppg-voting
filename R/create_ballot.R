# Script to generate code to make PPG ballot (form) on Google Forms

library(tidyverse)
library(glue)
library(lubridate)
library(gmailr)

source("R/functions.R")

# Set variables for this ballot period
ballot_number <- "5"
submission_period <- "September 2023"

# Rest of variables are set automatically
# 1 month for submission + 1 month for discussion + 1 month for voting
discussion_period <- next_month(submission_period)
voting_period <- next_month(discussion_period)
voting_deadline <- make_deadline(voting_period)
# cutoff should bracket submission period
# e.g., for September, start = 8/31, end = 10/1
cutoff_start <- submission_period %>%
  my() %>%
  floor_date("month") %>%
  magrittr::subtract(days(1)) %>%
  as.Date()
cutoff_end <- submission_period %>%
  my() %>%
  ceiling_date("month") %>%
  as.Date()

# Download list of issues (proposals)
issues <- fetch_issues("pteridogroup/ppg")

# Filter to proposals to be voted on
issues_to_vote <-
  issues |>
  separate(created_at, c("created_date", "created_time"), sep = "T") |>
  mutate(created_date = ymd(created_date)) |>
  filter(created_date > cutoff_start) |>
  filter(created_date < cutoff_end) |>
  filter(state == "open") |>
  arrange(number)

# Generate code to make form
add_proposal <- function(issue) {
  glue::glue('
  item = "{issue$number}: {issue$title}";
  var choices = ["Yes", "No", "Abstain"];
  var question = form.addMultipleChoiceItem()
  .setTitle(item)
  .setChoiceValues(choices)
  .setRequired(true);
  question.setHelpText("See: https://github.com/pteridogroup/ppg/issues/{issue$number}");\n
  ')
}

res <- list()
for (i in 1:nrow(issues_to_vote)) {
  res[i] <- add_proposal(issues_to_vote[i, ])
}

description <- paste(
  glue(
    "This ballot includes all valid proposals submitted during \\
    {submission_period}, which were available for discussion for at least \\
    one month ({discussion_period}), and are available for voting during \\
    {voting_period}."),
  glue(
    "You may vote as many times as you want until the deadline, \\
    {voting_deadline}. Only your most recent vote will be counted."),
  "No votes cast after the deadline will be counted.",
  glue(
    "Your vote will only be counted if the email address you entered matches \\
    your email address in the PPG mailing list. \\
    Please double check to make sure you are using the email address that is \\
    registered on the PPG mailing list, not another email address."),
  glue("Note that numbering of proposals is unique, but not consecutive \\
    (some numbers may be skipped)"),
  sep = "\\n\\n"
)

header <- glue::glue('
function createForm() {
   // create & name Form
   var item = "PPG Ballot [ballot_number]";
   var form = FormApp.create(item)
       .setTitle(item)
       .setDescription("[description]");

   // single line text field
   item = "GitHub username";
   form.addTextItem()
       .setTitle(item)
       .setRequired(false);

', .open = "[", .close = "]")

# Save code to make form
write_lines(
  c(
    header,
    unlist(res),
    "}"
  ), "results/voting-form-code.txt"
)

# Go to https://script.google.com/home, open "PPG Ballot" project
# (if not created already), and paste in code from voting-form-code.txt
# Click, "save", then "run", and open newly generated form in Google Forms
#
# Additional settings in Google Forms:
# Set "Collect email addresses" to "Responder input"
# Set "Send responders a copy of their response" to "Always"
#
# Reference youtube video: https://www.youtube.com/watch?v=L33hMxuoFtM

# Send email ----

# Set form URL after preparing form
form_url <- "https://forms.gle/Wdg53x23FzbSHxts6"

# Generate email text
ballot_announce_email <-
  gm_mime() %>%
  gm_to("ourPPG@googlegroups.com") %>%
  gm_from("ourPPG@googlegroups.com") %>%
  gm_subject(glue("PPG Ballot {ballot_number} Link")) %>%
  gm_html_body(
    glue(
      "<p><b>PPG Ballot {ballot_number} Link (Voting Period {voting_period})</b></p>",
      "<p>This is an automated email providing a link to the ballot for \\
      taxonomic proposals submitted to PPG \\
      (https://github.com/pteridogroup/ppg). It is sent to everyone on \\
      the PPG mailing list once per month. If you do not wish to receive it \\
      or have any questions, please contact Eric Schuettpelz \\
      (schuettpelze@si.edu) or Joel Nitta (joelnitta@gmail.com).</p>",
      "<p>DO NOT REPLY TO THIS EMAIL</br>; no response will be given.</p><hr>",
      "<p>Please fill out the ballot (Google Form) here: {form_url}</p>",
      "<p>You may vote as many times as you want until the deadline, \\
      <b>{voting_deadline}</b>. Only your most recent vote will be \\
      counted.</p>",
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
gm_create_draft(ballot_announce_email) # nolint

# or just send the existing MIME message
# gm_send_message(ballot_announce_email)
