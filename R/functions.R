fix_email <- function(data) {
  data |>
    dplyr::mutate(
      # Automatically fix typos in email address
      email = tolower(email) %>%
        stringr::str_replace_all("\\.ocm$", ".com") %>%
        stringr::str_replace_all("\\.combr$", ".com") %>%
        stringr::str_replace_all(
          "dar\\.sanin\\@gmail\\.com$",
          "dav.sanin@gmail.com"
        ) %>%
        stringr::str_replace_all(
          "fittmatos\\@gmail\\.com$",
          "fbittmatos@gmail.com"
        ) %>%
        stringr::str_replace_all("\\.utexxas\\.", ".utexas.") %>%
        stringr::str_replace_all(
          "ralf\\.knap\\@gmail\\.com",
          "ralf\\.knapp\\@gmail\\.com"
        )
    )
}

check_ballot <- function(
  ballot_file,
  email_file
) {
  # Load ballot ----
  ballot <- googlesheets4::read_sheet(ballot_file) |>
    dplyr::rename(timestamp = Timestamp, email = `Email Address`) |>
    fix_email()

  # Load and format email list -----

  # PPG mailing-list email is in column "email" and other emails are in column
  # "other email". Combine this into single email column.
  ppg_emails <- googlesheets4::read_sheet(email_file) |>
    janitor::clean_names()

  # combine primary and secondary emails
  # so that each name may have multiple email addresses
  ppg_emails <-
    ppg_emails |>
    dplyr::select(name, email) |>
    dplyr::bind_rows(
      dplyr::select(ppg_emails, name, email = secondary)
    ) |>
    dplyr::bind_rows(
      dplyr::select(ppg_emails, name, email = tertiary)
    ) |>
    dplyr::filter(!is.na(email)) |>
    unique() |>
    fix_email() |>
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
    # Add check for duplicated names of submitters
    # only keep most recent vote per person
    dplyr::arrange(name, dplyr::desc(timestamp)) |>
    dplyr::mutate(name_check = !duplicated(name))
}

tally_votes <- function(ballot_checked) {
  ballot_final <-
    ballot_checked |>
    dplyr::filter(email_check == TRUE) |>
    dplyr::filter(name_check == TRUE) |>
    dplyr::select(-dplyr::contains("check"))

  # Tally votes
  ballot_final |>
    dplyr::select(-timestamp, -email, -`GitHub username (optional)`, -name) |>
    tidyr::pivot_longer(
      names_to = "proposal",
      values_to = "response",
      tidyselect::everything()
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
  votes_tally_n_long <- votes_tally |>
    tidyr::pivot_wider(
      names_from = response,
      values_from = n,
      id_cols = proposal
    )

  if (!"No" %in% colnames(votes_tally_n_long)) {
    votes_tally_n_long <- dplyr::mutate(
      votes_tally_n_long,
      No = 0
    )
  }

  tally_n <-
    votes_tally_n_long |>
    dplyr::mutate(No = tidyr::replace_na(No, 0)) |>
    dplyr::rename(no_n = No, yes_n = Yes)

  votes_tally_p_long <- votes_tally |>
    tidyr::pivot_wider(
      names_from = response,
      values_from = percent,
      id_cols = proposal
    )

  if (!"No" %in% colnames(votes_tally_p_long)) {
    votes_tally_p_long <- dplyr::mutate(
      votes_tally_p_long,
      No = 0
    )
  }

  tally_p <-
    votes_tally_p_long |>
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
  votes_tally,
  ballot_number,
  vote_period
) {
  repo <- "https://github.com/pteridogroup/ppg/issues/"

  pivot_tally(votes_tally, ballot_number, vote_period) |>
    separate_wider_delim(
      cols = "proposal",
      delim = ": ",
      too_many = "merge",
      names = c("num", "proposal")
    ) %>%
    dplyr::mutate(
      num = parse_number(num),
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

# Extract useful information to dataframe
fetch_issues <- function(repo, n_max = 1000) {
  issues_json <-
    glue::glue(
      "https://api.github.com/repos/{repo}/issues?state=all&page=1&per_page={n_max}"
    ) |>
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

  if (nrow(issues_df) == n_max) {
    stop("Maximum number of issues fetched; increase n_max")
  }

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
        url,
        "https://api.github.com/repos/",
        "https://github.com/"
      ),
      name = stringr::str_match(body, "Name of taxon[\r|\n]*(×*.*)") |>
        magrittr::extract(, 2),
      rank = stringr::str_match(body, "Rank of taxon[\r|\n]*(\\w+)[\r|\n]*") |>
        magrittr::extract(, 2),
      no_species = stringr::str_match(
        body,
        "number of species affected[\r|\n]*(.*)"
      ) |>
        magrittr::extract(, 2),
      description = stringr::str_match(
        body,
        "Description of change[\r|\n]*(.*)"
      ) |>
        magrittr::extract(, 2)
    ) |>
    dplyr::select(-body)
}

# Filter proposals to those matching a particular status string
# and extract a single string of text to print in the email digest
proposal_df2txt <- function(proposals, status_search, ret_empty = "none") {
  text_compact <-
    proposals |>
    dplyr::filter(stringr::str_detect(status, status_search)) |>
    dplyr::pull(text) |>
    paste(collapse = "<br>")
  if (text_compact == "") {
    if (ret_empty == "none") {
      return("(none)")
    } else {
      return(NULL)
    }
  } else {
    return(text_compact)
  }
}

next_month <- function(month) {
  month %>%
    lubridate::my() %>%
    magrittr::add(months(1)) %>%
    format("%B %Y")
}


#' Make a deadline for a PPG ballot
#'
#' @param month Month (and year) of the voting period, ex: "October 2023".
#' @param timezone Timezone of the deadline.
#' @param for_google Logical; should this be formatted for pasting into a
#' Google script?.
#'
#' @return String
make_deadline <- function(month, timezone = "UTC", for_google = FALSE) {
  deadline <-
    month %>%
    # Parse the month to a date
    lubridate::my() %>%
    # Find the last day of the month
    ceiling_date("month") %>%
    magrittr::subtract(days(1)) %>%
    # Set time to 11:59PM
    update(hours = 23, minutes = 59, seconds = 59) %>%
    # Specify timezone. First set default to UTC, then convert from there
    with_tz("UTC") %>%
    with_tz(timezone)

  # Format the date to the desired output
  if (for_google) {
    deadline <- format(deadline, "%Y-%m-%d %H:%M")
  }
  if (!for_google) {
    deadline <- format(deadline, "%l:%M%p on %B %e, %Y %Z")
  }

  stringr::str_squish(deadline)
}

# Make tibble of commenters on an issue
fetch_commenters <- function(issue_num) {
  data <- gh(
    "GET /repos/pteridogroup/ppg/issues/{issue_num}/timeline",
    issue_num = issue_num
  )

  commenters <- purrr::map(data, ~ purrr::pluck(.x, "user", "login")) |>
    purrr::flatten() |>
    unlist()

  tibble::tibble(
    issue = issue_num,
    commenter = commenters
  )
}
