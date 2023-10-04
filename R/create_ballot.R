# Script to generate code to make PPG ballot (form) on Google Forms

library(tidyverse)
library(glue)

source("R/functions.R")

# Set variables for this ballot period
# 1 month for submission + 1 month for discussion + 1 month for voting
ballot_number <- "4"
submission_period <- "August 2023"
discussion_period <- "September 2023"
voting_period <- "October 2023"
voting_deadline <- "11:59PM on October 31, 2023 UTC+00:00"
# cutoff should bracket submission period
cutoff_start <- ymd("2023-07-31")
cutoff_end <- ymd("2023-09-01")

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
  item = "{issue$title}";
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
  glue("This ballot includes all valid proposals submitted during {submission_period}, which were available for discussion for at least one month ({discussion_period}), and are available for voting during {voting_period}."),
  glue("You may vote as many times as you want until the deadline, {voting_deadline}. Only your most recent vote will be counted."),
  "No votes cast after the deadline will be counted.",
  "Your vote will only be counted if the email address you entered matches your email address in the PPG mailing list.",
  "Please double check to make sure you are using the email address that is registered on the PPG mailing list, not another email address.",
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