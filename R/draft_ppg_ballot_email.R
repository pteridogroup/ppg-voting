#' Draft an email announcing a new PPG Ballot
#'
#' Requires that the pteridogroup.no.reply@gmail.com account has been
#' authenticated in the current working directory using client_secret.*json
#'
#' @param ballot_number What it says. Ballots are numbered sequentially starting
#'   from 1.
#' @param submission_period Period that proposals were submitted, formatted
#'   as month plus year, e.g. "January 2023".
#' @param form_url URL of the Google Form for the ballot.
#'
#' @return Nothing; check the pteridogroup.no.reply@gmail.com account to see
#' the email draft and send
draft_ppg_ballot_email <- function(ballot_number, submission_period, form_url) {
  discussion_period <- next_month(submission_period)
  voting_period <- next_month(discussion_period)
  voting_deadline <- make_deadline(voting_period, "UTC")

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
    ".secrets",
    pattern = "client_secret.*json", full.names = TRUE
  )
  gm_auth_configure(path = secret_json)
  gm_oauth_client()
  gm_auth("pteridogroup.no.reply@gmail.com")

  # Verify it looks correct, i.e. look at your Gmail drafts in the browser
  invisible(gm_create_draft(ballot_announce_email)) # nolint

  # or just send the existing MIME message
  # gm_send_message(ballot_announce_email)
}
