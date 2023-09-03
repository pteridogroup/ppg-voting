check_ballot <- function(
    ballot_file,
    ballot_cutoff,
    email_file) {
  # Load ballot ----
  ballot <- readr::read_csv(ballot_file, show_col_types = FALSE) |>
    dplyr::rename(timestamp = Timestamp, email = Username) |>
    dplyr::mutate(email = tolower(email)) |>
    # Convert time stamp to Greenwich time (same as UTC)
    dplyr::mutate(timestamp = stringr::str_remove(timestamp, " GMT\\+9")) |>
    dplyr::mutate(
      timestamp = lubridate::ymd_hms(timestamp, tz = "Greenwich") +
        lubridate::hours(9)
    ) |>
    # Add voting cutoff
    dplyr::mutate(cutoff = lubridate::ymd_hms(ballot_cutoff, tz = "Greenwich"))

  # Load and format email list -----

  # PPG mailing-list email is in column "email" and other emails are in column
  # "other email". Combine this into single email column.
  ppg_emails <- googlesheets4::read_sheet(email_file) |>
    janitor::clean_names()

  # combine primary and secondary emails
  # so that each name may have multiple email addresses
  ppg_emails <-
    ppg_emails |>
    dplyr::select(name, email = email_1) |>
    dplyr::bind_rows(
      dplyr::select(ppg_emails, name, email = email_2)
    ) |>
    dplyr::filter(!is.na(email)) |>
    dplyr::mutate(email = tolower(email)) |>
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

format_tally <- function(tally_final, ballot_number, vote_period) {
  tally_n <-
    tally_final |>
    tidyr::pivot_wider(
      names_from = response, values_from = n, id_cols = proposal
    ) |>
    dplyr::mutate(No = tidyr::replace_na(No, 0)) |>
    dplyr::rename(no_n = No, yes_n = Yes)

  tally_p <-
    tally_final |>
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
    ) |>
    dplyr::mutate(
      text = glue::glue(
        "This proposal was voted on during PPG Ballot {ballot_number} (voting period {vote_period}). A total of {total} votes were cast. There were {yes_n} 'Yes' votes ({yes_p}%) and {no_n} 'No' votes ({no_p}%). The proposal {result}." # nolint
      )
    ) |>
    dplyr::select(proposal, text)
}

# Extract useful information to dataframe
fetch_issues <- function(repo) {

  issues_json <-
    glue::glue("https://api.github.com/repos/{repo}/issues?state=all") |>
    jsonlite::fromJSON()

  tibble::tibble(
    number = issues_json$number,
    title = issues_json$title,
    url = issues_json$url,
    created_at = issues_json$created_at,
    user = issues_json$user$login,
    state = issues_json$state,
    body = issues_json$body,
    # draft = issues_json$draft # draft only applies to PRs, use if PRs exist
  ) |>
    # Remove PRs: issues have NA for draft
    # filter(is.na(draft)) %>%
    # select(-draft) %>%
  dplyr::mutate(
    url = stringr::str_replace_all(
      url, "https://api.github.com/repos/", "https://github.com/"),
    name = stringr::str_match(body, "Name of taxon\n\n(.*)") |>
             magrittr::extract(, 2),
    rank = stringr::str_match(body, "Rank of taxon\n\n(.*)") |>
             magrittr::extract(, 2),
    no_species = stringr::str_match(
      body, "number of species affected\n\n(.*)") |>
             magrittr::extract(, 2),
    description = stringr::str_match(body, "Description of change\n\n(.*)") |>
      magrittr::extract(, 2)
  ) |>
    dplyr::select(-body)
}