
#' Generate a script to make a PPG Ballot as a Google Form
#'
#' This does not create a Google Form by itself, but rather generates a JSON
#' script that can be pasted into the Google Apps Script to generate a Google
#' Form
#'
#' @param ballot_number What it says. Ballots are numbered sequentially starting
#'   from 1.
#' @param submission_period Period that proposals were submitted, formatted
#'   as month plus year, e.g. "January 2023".
#' @param script_path Path to write the script that generates the Google Form.
#'
#' @return Nothing; run for its side effect (generating the script)
generate_form_script <- function(
    ballot_number, submission_period, script_path) {
  # Rest of variables are set automatically
  # 1 month for submission + 1 month for discussion + 1 month for voting
  discussion_period <- next_month(submission_period)
  voting_period <- next_month(discussion_period)
  voting_deadline <- make_deadline(voting_period, "UTC")
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
    {voting_period}."
    ),
    glue(
      "You may vote as many times as you want until the deadline, \\
    {voting_deadline}. Only your most recent vote will be counted."
    ),
    glue(
      "Your vote will only be counted if the email address you entered matches \\
    your email address in the PPG mailing list. \\
    Please double check to make sure you are using the email address that is \\
    registered on the PPG mailing list, not another email address."
    ),
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
   // Set the form to collect email addresses
       form.setCollectEmail(true);
   // single line text field
   item = "GitHub username (optional)";
   form.addTextItem()
       .setTitle(item)
       .setRequired(false);

', .open = "[", .close = "]")

  # Save code to make form
  invisible(
    write_lines(
    c(
      header,
      unlist(res),
      "}"
    ), script_path
    )
  )
}
