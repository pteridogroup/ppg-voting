# Script to generate code to make PPG ballot (form) on Google Forms

library(tidyverse)
library(glue)
library(lubridate)
library(gmailr)

source("R/functions.R")
source("R/generate_form_script.R")
source("R/draft_ppg_ballot_email.R")

# 0: Set variables for this ballot ---
ballot_number <- "17"
submission_period <- "May 2025"
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

# Set a closing timer to automatically close form
#
# Will need to set deadline in local time (e.g, Japan), so check this first:
# Japan = GMT+9 # nolint
make_deadline(voting_period, "Japan", for_google = TRUE)

# - Open the new form
# - From the form menu, click "Apps Script"
# - Copy and paste the code from `close_form.js` in this repo
# - Save the script
# - Hit the "Run" button to test and authorize (will close form)
# - Re-open form
# - Set a timer to run the script (OK the warnings)
#  - Click "Triggers" (the clock icon)
#  - Click "Add trigger" (bottom RHC)
#    - Set "event source" to "Time-driven"
#    - Set "type of time based trigger" to "Specific date and time"
#    - Enter deadline that you checked with make_deadline()
#      NOTING THE TIME ZONE (Japan is GMT+9:00)
#    - Save
#    - Go through authentication again, if it pops up
#    - You should see the new trigger added to the list of triggers
#
# Reference tutorial: https://web-breeze.net/en/auto-close-google-forms/

# 4: Draft email ----

# This creates a draft email to send from pteridogroup.no.reply@gmail.com,
# but does not actually send it yet (so it can be manually checked first)

draft_ppg_ballot_email(
  ballot_number = ballot_number,
  submission_period = submission_period,
  form_url = "https://forms.gle/XYCtw7cNy81oJT44A" # From form created in Step 2
)

# Open pteridogroup.no.reply@gmail.com account, check drafts, and send
