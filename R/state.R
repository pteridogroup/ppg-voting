# Track pipeline progress across runs so ballot number, submission period,
# and vote period never need to be typed in by hand. Also prevents a busy
# or skipped month from silently vanishing: the next period to check is
# always "last checked + 1 month", never "the current calendar month".
#
# Ballot numbers and submission periods are tracked separately: a month
# with no eligible proposals advances last_checked_submission_period but
# NOT last_completed_ballot_number, so a ballot number is only ever spent
# on a period that actually went out for a vote.

read_ballot_state <- function(path) {
  yaml::read_yaml(path)
}

write_ballot_state <- function(state, path) {
  yaml::write_yaml(state, path)
}

add_month <- function(date) {
  lubridate::floor_date(date, "month") %>% magrittr::add(months(1))
}

period_key <- function(period_string) {
  format(lubridate::my(period_string), "%Y-%m-%d")
}

warn_if_backlogged <- function(next_period, stage) {
  last_full_month <-
    lubridate::floor_date(lubridate::today("UTC"), "month") %>%
    magrittr::subtract(months(1))
  if (next_period < last_full_month) {
    warning(
      stage, " is more than one month behind: next period is ",
      format(next_period, "%B %Y"), ", but the most recently completed ",
      "calendar month is ", format(last_full_month, "%B %Y"), ". ",
      "Run the pipeline again after this one to catch up the remaining ",
      "backlog.",
      call. = FALSE
    )
  }
}

#' Determine ballot number, submission/discussion/voting period for the
#' next ballot to be created. The period advances one month at a time
#' regardless of whether prior months had eligible proposals; the ballot
#' number only advances when a month actually produces a ballot.
compute_next_creation <- function(state) {
  next_period <- add_month(as.Date(state$creation$last_checked_submission_period))
  warn_if_backlogged(next_period, "Ballot creation")

  submission_period <- format(next_period, "%B %Y")
  discussion_period <- next_month(submission_period)
  voting_period <- next_month(discussion_period)

  list(
    ballot_number = state$creation$last_completed_ballot_number + 1,
    submission_period = submission_period,
    discussion_period = discussion_period,
    voting_period = voting_period
  )
}

#' TRUE if a ballot's form was never created because it had no eligible
#' proposals (see create_ballot_form())
is_skipped <- function(form_meta) {
  is.null(form_meta$form_id) || isTRUE(form_meta$skipped)
}

#' TRUE once the calendar month named by a "Month YYYY" period string is over
period_has_ended <- function(period_string) {
  lubridate::today("UTC") >= add_month(lubridate::my(period_string))
}

#' Find the next real ballot (skipped/empty periods don't count) that's
#' waiting to be tallied AND whose voting period has actually closed, i.e.
#' the lowest ballot_number recorded under creation$forms that's greater
#' than tally$last_completed_ballot_number.
#'
#' Deliberately does NOT compute this from a fixed monthly cadence: since
#' ballot creation can skip empty months, the Nth ballot to be tallied is
#' whatever ballot creation actually produced next, not "N months after
#' the last tally". Returns NULL if nothing is ready yet (e.g. the next
#' ballot hasn't been created, or its voting period hasn't closed).
find_next_tally <- function(ballot_state) {
  real_forms <- purrr::keep(ballot_state$creation$forms, ~ !is.null(.x$ballot_number))
  last_tallied <- ballot_state$tally$last_completed_ballot_number
  candidates <- purrr::keep(real_forms, ~ .x$ballot_number > last_tallied)

  if (length(candidates) == 0) {
    return(NULL)
  }

  ballot_numbers <- purrr::map_dbl(candidates, "ballot_number")
  next_form <- candidates[[which.min(ballot_numbers)]]

  if (!period_has_ended(next_form$voting_period)) {
    message(
      "Ballot ", next_form$ballot_number, "'s voting period (",
      next_form$voting_period, ") hasn't ended yet; nothing to tally."
    )
    return(NULL)
  }

  if (length(candidates) > 1) {
    warning(
      length(candidates), " ballots are waiting to be tallied (numbers ",
      paste(sort(ballot_numbers), collapse = ", "), "); tallying only the ",
      "oldest one this run. Run the pipeline again to catch up the rest.",
      call. = FALSE
    )
  }

  next_form
}

#' Record that a submission period had no eligible proposals, so no ballot
#' number or Google Form was used for it. Idempotent per period.
record_skipped_period <- function(state_path, submission_period) {
  state <- read_ballot_state(state_path)
  key <- period_key(submission_period)

  existing <- state$creation$forms[[key]]
  if (!is.null(existing)) {
    return(existing)
  }

  form_meta <- list(
    ballot_number = NULL, form_id = NULL, form_url = NULL, skipped = TRUE
  )
  state$creation$forms[[key]] <- form_meta
  state$creation$last_checked_submission_period <- key
  write_ballot_state(state, state_path)
  form_meta
}

#' Record that a ballot's Google Form has been created, storing everything
#' needed to fetch and interpret its responses later. Idempotent: safe to
#' call again for a period that's already recorded (returns the existing
#' metadata instead of creating a duplicate form).
record_created_form <- function(state_path, ballot_number, submission_period, form_meta) {
  state <- read_ballot_state(state_path)
  key <- period_key(submission_period)

  existing <- state$creation$forms[[key]]
  if (!is.null(existing)) {
    return(existing)
  }

  form_meta$ballot_number <- ballot_number
  if (is.null(form_meta$announced)) {
    form_meta$announced <- FALSE
  }
  if (is.null(form_meta$closed)) {
    form_meta$closed <- FALSE
  }
  state$creation$forms[[key]] <- form_meta
  state$creation$last_completed_ballot_number <- ballot_number
  state$creation$last_checked_submission_period <- key
  write_ballot_state(state, state_path)
  form_meta
}

#' Find all real ballots that have a form but haven't had their
#' announcement email drafted yet (covers both a normal fresh ballot and
#' recovering from a crash between form creation and drafting the email)
find_unannounced <- function(ballot_state) {
  real_forms <- purrr::keep(ballot_state$creation$forms, ~ !is.null(.x$ballot_number))
  purrr::keep(real_forms, ~ !isTRUE(.x$announced))
}

#' Record that a ballot's announcement email has been drafted
mark_announced <- function(state_path, submission_period) {
  state <- read_ballot_state(state_path)
  key <- period_key(submission_period)
  state$creation$forms[[key]]$announced <- TRUE
  write_ballot_state(state, state_path)
  invisible(state)
}

#' Find all real ballots whose voting period has ended but that haven't
#' been closed to new responses yet
find_forms_to_close <- function(ballot_state) {
  real_forms <- purrr::keep(ballot_state$creation$forms, ~ !is.null(.x$ballot_number))
  open_forms <- purrr::keep(real_forms, ~ !isTRUE(.x$closed))
  purrr::keep(open_forms, ~ period_has_ended(.x$voting_period))
}

#' Print the closing deadline (in local_timezone) of whichever real
#' ballot is currently open, as a reminder for manually setting the
#' native Google Forms closing timer under Settings > Responses.
report_current_ballot_deadline <- function(ballot_state, local_timezone = "Asia/Tokyo") {
  real_forms <- purrr::keep(ballot_state$creation$forms, ~ !is.null(.x$ballot_number))
  open_forms <- purrr::keep(real_forms, ~ !isTRUE(.x$closed))

  if (length(open_forms) == 0) {
    message("No ballot is currently open.")
    return(invisible(NULL))
  }

  ballot_numbers <- purrr::map_dbl(open_forms, "ballot_number")
  current <- open_forms[[which.min(ballot_numbers)]]

  message(
    "Ballot ", current$ballot_number, " (voting period ",
    current$voting_period, ") closes at ",
    make_deadline(current$voting_period, local_timezone), " (",
    make_deadline(current$voting_period, "UTC"), ")."
  )
  invisible(current)
}

#' Record that a ballot's form has been closed to new responses
mark_closed <- function(state_path, submission_period) {
  state <- read_ballot_state(state_path)
  key <- period_key(submission_period)
  state$creation$forms[[key]]$closed <- TRUE
  write_ballot_state(state, state_path)
  invisible(state)
}

#' Record that a ballot has been tallied
mark_tally_complete <- function(state_path, ballot_number, vote_period) {
  state <- read_ballot_state(state_path)
  state$tally$last_completed_ballot_number <- ballot_number
  state$tally$last_completed_vote_period <- period_key(vote_period)
  write_ballot_state(state, state_path)
  invisible(state)
}
