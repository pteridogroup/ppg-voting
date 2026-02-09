# Script to generate code to make PPG ballot (form) on Google Forms

library(tidyverse)
library(glue)
library(lubridate)
library(gmailr)

source("R/functions.R")
source("R/generate_form_script.R")
source("R/draft_ppg_ballot_email.R")

# 0: Set variables for this ballot ---
ballot_number <- "22"
submission_period <- "December 2025"
discussion_period <- next_month(submission_period)
voting_period <- next_month(discussion_period)

# 1: Make script ----

# Generate JSON script to make Google Form for current ballot
generate_form_script(
  ballot_number = ballot_number,
  submission_period = submission_period,
  script_path = "results/voting-form-code.txt"
)

# 2: Create Google Form ----

# Go to https://script.google.com/home, open "PPG Ballot" project
# (if not created already), and paste in code from voting-form-code.txt
# Click, "save", then "run", and open newly generated form in Google Forms
#
# Additional settings in Google Forms:
# Set "Send responders a copy of their response" to "Always"
#
# Reference youtube video: https://www.youtube.com/watch?v=L33hMxuoFtM

# 3: Set closing timer -----

# Set a closing timer to close form on deadline, set under "Publishing Options"
#
# Will need to set deadline in local time (e.g, Japan), so check this first:
# Japan = GMT+9 # nolint
# America/Los_Angeles = GMT - 7 (during Daylight savings)
make_deadline(voting_period, "Japan", for_google = TRUE)

# 4: Draft email ----

# This creates a draft email to send from pteridogroup.no.reply@gmail.com,
# but does not actually send it yet (so it can be manually checked first)

draft_ppg_ballot_email(
  ballot_number = ballot_number,
  submission_period = submission_period,
  form_url = "https://forms.gle/gGe8vddaK43gPHPb9" # From form created in Step 2
)

# Open pteridogroup.no.reply@gmail.com account, check drafts, and send
