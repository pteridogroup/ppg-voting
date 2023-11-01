check_ballot <- function(
    ballot_file,
    ballot_cutoff,
    email_file) {
  # Load ballot ----
  ballot <- googlesheets4::read_sheet(ballot_file) |>
    dplyr::rename(timestamp = Timestamp, email = `Email Address`) |>
    dplyr::mutate(
      email = tolower(email),
      # Automatically fix typo in email address "ocm" instead of "com"
      email = stringr::str_replace_all(email, "\\.ocm$", ".com")
    ) |>
    # Convert time stamp to Greenwich time (same as UTC)
    dplyr::mutate(timestamp = stringr::str_remove(timestamp, " GMT\\+9")) |>
    dplyr::mutate(
      timestamp = lubridate::ymd_hms(timestamp, tz = "Greenwich") +
        lubridate::hours(9)
    ) |>
    # Add voting cutoff
    dplyr::mutate(
      cutoff = lubridate::ymd_hms(ballot_cutoff, tz = "Greenwich")) |>
    assertr::assert(assertr::not_na, cutoff)


  # Load and format email list -----

  # PPG mailing-list email is in column "email" and other emails are in column
  # "other email". Combine this into single email column.
  ppg_emails <- googlesheets4::read_sheet(email_file) |>
    janitor::clean_names()

  # combine primary and secondary emails
  # so that each name may have multiple email addresses
  ppg_emails <-
    ppg_emails |>
    dplyr::select(name, email = primary_our_ppg) |>
    dplyr::bind_rows(
      dplyr::select(ppg_emails, name, email = email_2)
    ) |>
    dplyr::bind_rows(
      dplyr::select(ppg_emails, name, email = email_3)
    ) |>
    dplyr::filter(!is.na(email)) |>
    dplyr::mutate(
      email = tolower(email),
      # Automatically fix typo in email address "ocm" instead of "com"
      email = stringr::str_replace_all(email, "\\.ocm$", ".com")
    ) |>
    unique() |>
    assertr::assert(assertr::is_uniq, email) |>
    assertr::assert(assertr::not_na, name)

  # Conduct checks
  ballot |>
    # Add check that all emails are in PPG list
    dplyr::left_join(
      dplyr::select(ppg_emails, name = name, email = email),
      by = "email"
    ) |>
    dplyr::mutate(email_check = !is.na(name)) |>
    # Add check that all emails pass time stamp cutoff
    dplyr::mutate(time_check = timestamp < cutoff) |>
    # Add check for duplicated names of submitters
    # only keep most recent vote per person
    dplyr::arrange(name, dplyr::desc(timestamp)) |>
    dplyr::mutate(name_check = !duplicated(name))
}

tally_votes <- function(ballot_checked) {
  ballot_final <-
    ballot_checked |>
    dplyr::filter(email_check == TRUE) |>
    dplyr::filter(time_check == TRUE) |>
    dplyr::filter(name_check == TRUE) |>
    dplyr::select(-dplyr::contains("check"))

  # Tally votes
  ballot_final |>
    dplyr::select(-timestamp, -email, -`GitHub username`, -cutoff, -name) |>
    tidyr::pivot_longer(
      names_to = "proposal", values_to = "response", tidyselect::everything()
    ) |>
    dplyr::group_by(proposal) |>
    dplyr::count(response) |>
    dplyr::filter(response != "Abstain") |>
    dplyr::mutate(total = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(percent = (n / total) * 100) |>
    dplyr::mutate(percent = round(percent, 1))
}

pivot_tally <- function(votes_tally, ballot_number, vote_period) {
  tally_n <-
    votes_tally |>
    tidyr::pivot_wider(
      names_from = response, values_from = n, id_cols = proposal
    ) |>
    dplyr::mutate(No = tidyr::replace_na(No, 0)) |>
    dplyr::rename(no_n = No, yes_n = Yes)

  tally_p <-
    votes_tally |>
    tidyr::pivot_wider(
      names_from = response, values_from = percent, id_cols = proposal
    ) |>
    dplyr::mutate(No = tidyr::replace_na(No, 0)) |>
    dplyr::rename(no_p = No, yes_p = Yes)

  dplyr::left_join(tally_n, tally_p, by = "proposal") |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(no_n, yes_n, na.rm = TRUE)) |>
    dplyr::mutate(
      min_to_pass = round((2 / 3) * total, 0),
      result = dplyr::if_else(yes_n > min_to_pass, "passes", "does not pass")
    ) %>%
    ungroup()
}

format_tally_github <- function(votes_tally, ballot_number, vote_period) {
   pivot_tally(votes_tally, ballot_number, vote_period) |>
    dplyr::mutate(
      text = glue::glue(
        "This proposal was voted on during PPG Ballot {ballot_number} \\
        (voting period {vote_period}). A total of {total} votes were cast. \\
        There were {yes_n} 'Yes' votes ({yes_p}%) and {no_n} 'No' votes \\
        ({no_p}%). The proposal {result}."
      )
    ) |>
    dplyr::select(proposal, text)
}

format_tally_email <- function(
  votes_tally, ballot_number, vote_period, issue_nums) {
  
  repo <- "https://github.com/pteridogroup/ppg/issues/"

  pivot_tally(votes_tally, ballot_number, vote_period) |>
    dplyr::left_join(
      issue_nums, by = "proposal"
    ) |>
    dplyr::mutate(
      text = glue::glue(
        "<li><a href = \"{repo}{num}\">{num}. {proposal}</a>: {yes_n} 'Yes' \\
        votes ({yes_p}%) and {no_n} 'No' votes ({no_p}%). \\
        The proposal {result}."
      )
    ) %>%
    arrange(desc(num)) %>%
    pull(text) %>%
    paste(collapse = "")
}

format_bad_time_email <- function(bad_time) {
  if (nrow(bad_time) == 0) {
    return("")
  }
  # Get num of bad times
  n_bad_time <- nrow(bad_time)
  # Formatting
  n_bad_time_e <- english::english(n_bad_time)
  cutoff <- bad_time %>%
    select(cutoff) %>%
    unique() %>%
    pull(cutoff) %>%
    format(., "%Y-%m-%d %H:%M:%S %Z")

  if (n_bad_time == 1) {
    res <- glue::glue(
      "<p>There was one vote cast after the deadline ({cutoff}) that could not \\
      be counted. Please be sure to vote on time.</p>"
    )
  } else if (n_bad_time > 1) {
    res <- glue::glue(
      "<p>There were {n_bad_time_e} votes cast after the deadline ({cutoff}) \\
      that could not be counted. Please be sure to vote on time.</p>"
    )
  } 
  return(res)
}

# Extract useful information to dataframe
fetch_issues <- function(repo) {

  issues_json <-
    glue::glue("https://api.github.com/repos/{repo}/issues?state=all") |>
    jsonlite::fromJSON()

  # Create initial tibble of issues (may include PRs)
  issues_df <- tibble::tibble(
    number = issues_json$number,
    title = issues_json$title,
    url = issues_json$url,
    created_at = issues_json$created_at,
    user = issues_json$user$login,
    state = issues_json$state,
    body = issues_json$body
  )

  # If any PRs exist, remove them
  if (!is.null(issues_json$draft)) {
    issues_df <-
      issues_df |>
      dplyr::mutate(draft = issues_json$draft) |>
      dplyr::filter(is.na(draft)) |>
      dplyr::select(-draft)
  }

  # Format final data frame
  issues_df |>
    dplyr::mutate(
    url = stringr::str_replace_all(
      url, "https://api.github.com/repos/", "https://github.com/"),
    name = stringr::str_match(body, "Name of taxon[\r|\n]*(×*\\w+)") |>
             magrittr::extract(, 2),
    rank = stringr::str_match(body, "Rank of taxon[\r|\n]*(\\w+)[\r|\n]*") |>
             magrittr::extract(, 2),
    no_species = stringr::str_match(
      body, "number of species affected[\r|\n]*(.*)") |>
       magrittr::extract(, 2),
    description = stringr::str_match(
      body, "Description of change[\r|\n]*(.*)") |>
        magrittr::extract(, 2)
  ) |>
    dplyr::select(-body)
}

# Filter proposals to those matching a particular status string
# and extract a single string of text to print in the email digest
proposal_df2txt <- function(proposals, status_search) {
  text_compact <-
    proposals |>
      dplyr::filter(stringr::str_detect(status, status_search)) |>
      dplyr::pull(text) |>
      paste(collapse = "<br>")
  if (text_compact == "") {
    return("(none)")
  } else {
    return(text_compact)
  }
}