#' Draft an email announcing the results of a PPG Ballot
#'
#' Requires that the pteridogroup.no.reply@gmail.com account has been
#' authenticated in the current working directory using client_secret.*json
#'
#' @param votes_tally Tally of votes (dataframe)
#' @param ballot_number What it says. Ballots are numbered sequentially starting
#'   from 1.
#' @param vote_period Period that proposals were voted on, formatted
#'   as month plus year, e.g. "January 2023".
#'
#' @return Nothing; check the pteridogroup.no.reply@gmail.com account to see
#' the email draft and send
draft_ppg_results_email <- function(votes_tally, ballot_number, vote_period) {

  vote_results_email_list <- format_tally_email(
    votes_tally, ballot_number, vote_period) 

  vote_results_email <-
    gm_mime() %>%
    gm_to("ourPPG@googlegroups.com") %>%
    gm_from("ourPPG@googlegroups.com") %>%
    gm_subject(glue("PPG Ballot {ballot_number} Results")) %>%
    gm_html_body(
      glue(
        "<p><b>PPG Ballot {ballot_number} Results (Voting Period {vote_period})</b></p>",
        "<p>This is an automated email summarizing the results of voting on \\
        taxonomic proposals submitted to PPG \\
        (https://github.com/pteridogroup/ppg). It is sent to everyone on \\
        the PPG mailing list once per month. If you do not wish to receive it \\
        or have any questions, please contact Eric Schuettpelz \\
        (schuettpelze@si.edu) or Joel Nitta (joelnitta@gmail.com).</p>",
        "<p>Please note the following:</p> \\
        <ul> \\
        <li>Numbering of proposals is unique, but not consecutive (some \\
        numbers may be skipped). \\
        <li>A 2/3 majority is required to pass \\
        </ul>",
        "<p>DO NOT REPLY TO THIS EMAIL</br>; no response will be given.</p><hr>",
        "<ul>{vote_results_email_list}</ul>",
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
  invisible(gm_create_draft(vote_results_email))

  # or just send the existing MIME message
  # gm_send_message(digest_email)
}
