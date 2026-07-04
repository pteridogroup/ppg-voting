# Create and read PPG Ballot Google Forms via the Forms API, replacing the
# old manual flow of pasting generated Apps Script into script.google.com.
#
# One-time setup required before this works:
#   1. In the Google Cloud project behind .secrets/client_secret*.json,
#      enable the "Google Forms API" (in addition to Gmail/Sheets already
#      enabled).
#   2. Consent to the new forms.body / forms.responses.readonly scopes
#      for pteridogroup.no.reply@gmail.com BEFORE running
#      targets::tar_make(): run `source("R/google_forms.R"); forms_token()`
#      directly in your live R console and complete the browser consent
#      flow there. tar_make() runs each target in an isolated background
#      subprocess by default (interactive() is FALSE there), so the
#      browser-based consent flow can't happen inside it; once the token
#      is cached in .secrets, tar_make()'s subprocesses can reuse it
#      without needing to prompt.

forms_scopes <- c(
  "https://www.googleapis.com/auth/forms.body",
  "https://www.googleapis.com/auth/forms.responses.readonly"
)

forms_token <- function() {
  options(gargle_oauth_cache = ".secrets")
  secret_json <- list.files(
    ".secrets",
    pattern = "client_secret.*json",
    full.names = TRUE
  )
  client <- gargle::gargle_oauth_client_from_json(secret_json, name = "ppg-forms")
  token <- gargle::token_fetch(
    scopes = forms_scopes,
    client = client,
    email = "pteridogroup.no.reply@gmail.com",
    cache = ".secrets"
  )

  if (is.null(token)) {
    stop(
      "Could not obtain a Google Forms API token (gargle::token_fetch() ",
      "returned NULL). This almost always means no cached token exists yet ",
      "and the current process can't open an interactive browser consent ",
      "flow (e.g. because it's running inside targets::tar_make()'s ",
      "isolated subprocess). Run `source(\"R/google_forms.R\"); ",
      "forms_token()` directly in your live R console first to complete ",
      "the one-time consent and cache the token, then re-run tar_make().",
      call. = FALSE
    )
  }

  token
}

gforms_error_body <- function(resp) {
  err <- tryCatch(httr2::resp_body_json(resp)$error, error = function(e) NULL)
  if (is.null(err)) {
    return(NULL)
  }
  glue::glue("{err$status}: {err$message}")
}

gforms_request <- function(method, path, token, body = NULL) {
  req <-
    httr2::request(paste0("https://forms.googleapis.com/v1/", path)) |>
    httr2::req_method(method) |>
    httr2::req_auth_bearer_token(token$credentials$access_token) |>
    httr2::req_error(body = gforms_error_body)

  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }

  httr2::req_perform(req) |>
    httr2::resp_body_json()
}

ballot_form_description <- function(meta) {
  voting_deadline <- make_deadline(meta$voting_period, "UTC")
  paste(
    glue::glue(
      "This ballot includes all valid proposals submitted during ",
      "{meta$submission_period}, which were available for discussion for ",
      "at least one month ({meta$discussion_period}), and are available ",
      "for voting during {meta$voting_period}."
    ),
    glue::glue(
      "You may vote as many times as you want until the deadline, ",
      "{voting_deadline}. Only your most recent vote will be counted."
    ),
    glue::glue(
      "Your vote will only be counted if the email address you entered ",
      "matches your email address in the PPG mailing list. Please double ",
      "check to make sure you are using the email address that is ",
      "registered on the PPG mailing list, not another email address."
    ),
    "Note that numbering of proposals is unique, but not consecutive (some numbers may be skipped)", # nolint
    sep = "\n\n"
  )
}

proposal_create_item_requests <- function(issues_to_vote) {
  purrr::map(seq_len(nrow(issues_to_vote)), function(i) {
    issue <- issues_to_vote[i, ]
    list(
      createItem = list(
        item = list(
          title = glue::glue("{issue$number}: {issue$title}"),
          description = glue::glue(
            "See: https://github.com/pteridogroup/ppg/issues/{issue$number}"
          ),
          questionItem = list(question = list(
            required = TRUE,
            choiceQuestion = list(
              type = "RADIO",
              options = list(
                list(value = "Yes"),
                list(value = "No"),
                list(value = "Abstain")
              )
            )
          ))
        ),
        location = list(index = i)
      )
    )
  })
}

extract_question_map <- function(batch_result, issues_to_vote) {
  replies <- batch_result$replies
  # replies[[1]] = updateFormInfo, replies[[2]] = updateSettings,
  # replies[[3]] = GitHub username item, replies[[4:]] = one per proposal
  # A CreateItemResponse only has itemId/questionId, not a nested item -
  # accessing $item here would partial-match $itemId instead.
  github_username <- replies[[3]]$createItem$questionId[[1]]

  proposals <- purrr::map2(
    replies[-(1:3)],
    seq_len(nrow(issues_to_vote)),
    function(reply, i) {
      list(
        question_id = reply$createItem$questionId[[1]],
        number = issues_to_vote$number[i],
        title = issues_to_vote$title[i]
      )
    }
  )
  names(proposals) <- as.character(issues_to_vote$number)

  list(github_username = github_username, proposals = proposals)
}

#' Create the Google Form for a ballot via the Forms API
#'
#' Idempotent: if this submission period is already recorded in
#' ballot_state.yaml (whether it produced a real form or was skipped for
#' having no eligible proposals), returns the existing metadata instead of
#' creating a second, duplicate form.
create_ballot_form <- function(meta, issues_to_vote, state_path) {
  state <- read_ballot_state(state_path)
  existing <- state$creation$forms[[period_key(meta$submission_period)]]
  if (!is.null(existing)) {
    return(existing)
  }

  if (nrow(issues_to_vote) == 0) {
    message(
      "No eligible taxonomic proposals for ", meta$submission_period, "; ",
      "no Google Form or ballot number will be used for this period. ",
      "Run the pipeline again to move on to the next period."
    )
    return(record_skipped_period(state_path, meta$submission_period))
  }

  token <- forms_token()
  title <- glue::glue("PPG Ballot {meta$ballot_number}")

  form <- gforms_request(
    "POST", "forms",
    token = token,
    body = list(info = list(title = title, documentTitle = title))
  )

  requests <- c(
    list(
      list(updateFormInfo = list(
        info = list(description = ballot_form_description(meta)),
        updateMask = "description"
      )),
      list(updateSettings = list(
        settings = list(emailCollectionType = "RESPONDER_INPUT"),
        updateMask = "emailCollectionType"
      )),
      list(createItem = list(
        item = list(
          title = "GitHub username (optional)",
          questionItem = list(question = list(
            required = FALSE,
            textQuestion = list(paragraph = FALSE)
          ))
        ),
        location = list(index = 0)
      ))
    ),
    proposal_create_item_requests(issues_to_vote)
  )

  batch_result <- gforms_request(
    "POST", glue::glue("forms/{form$formId}:batchUpdate"),
    token = token,
    body = list(requests = requests, includeFormInResponse = TRUE)
  )

  form_meta <- list(
    form_id = form$formId,
    form_url = batch_result$form$responderUri,
    edit_url = glue::glue("https://docs.google.com/forms/d/{form$formId}/edit"),
    submission_period = meta$submission_period,
    voting_period = meta$voting_period,
    questions = extract_question_map(batch_result, issues_to_vote)
  )

  message(
    "Created ", title, " at ", form_meta$form_url, ". It will close to new ",
    "responses automatically once its voting period (", meta$voting_period,
    ") ends. The Forms API has no setting for \"send responders a copy of ",
    "their response\", so if you want that, turn it on manually under ",
    "Settings > Responses."
  )

  record_created_form(state_path, meta$ballot_number, meta$submission_period, form_meta)
}

#' Draft the announcement email for every real ballot that has a form but
#' hasn't been announced yet. Deliberately independent of which period
#' next_creation is currently working on, so it also catches a ballot
#' whose announcement was missed or failed in an earlier run.
announce_pending_ballots <- function(ballot_state, state_path) {
  pending <- find_unannounced(ballot_state)

  purrr::walk(pending, function(form_meta) {
    draft_ppg_ballot_email(
      ballot_number = form_meta$ballot_number,
      submission_period = form_meta$submission_period,
      form_url = form_meta$form_url
    )
    mark_announced(state_path, form_meta$submission_period)
  })

  invisible(pending)
}

#' Stop a form from accepting new responses via forms.setPublishSettings.
#' Keeps the form published (isPublished stays TRUE) - only turns off
#' isAcceptingResponses.
close_ballot_form <- function(form_id, token) {
  gforms_request(
    "POST", glue::glue("forms/{form_id}:setPublishSettings"),
    token = token,
    body = list(
      publishSettings = list(publishState = list(
        isPublished = TRUE,
        isAcceptingResponses = FALSE
      )),
      updateMask = "publish_state"
    )
  )
}

#' Close every real ballot whose voting period has ended but that's still
#' open to responses. Deliberately independent of the tally step: closing
#' just stops new submissions, it doesn't require the ballot to have been
#' tallied yet.
close_expired_ballots <- function(ballot_state, state_path) {
  to_close <- find_forms_to_close(ballot_state)
  if (length(to_close) == 0) {
    return(invisible(to_close))
  }

  token <- forms_token()
  purrr::walk(to_close, function(form_meta) {
    close_ballot_form(form_meta$form_id, token)
    mark_closed(state_path, form_meta$submission_period)
  })

  invisible(to_close)
}

fetch_form_responses <- function(form_id, token) {
  responses <- list()
  page_token <- NULL
  repeat {
    path <- glue::glue("forms/{form_id}/responses")
    if (!is.null(page_token)) {
      path <- paste0(path, "?pageToken=", page_token)
    }
    result <- gforms_request("GET", path, token = token)
    responses <- c(responses, result$responses)
    page_token <- result$nextPageToken
    if (is.null(page_token)) break
  }
  responses
}

#' Convert raw Forms API responses into the same shape check_ballot()
#' has always expected: one row per submission, one column per proposal
form_responses_to_ballot <- function(responses, form_meta) {
  if (length(responses) == 0) {
    proposal_cols <- purrr::map(form_meta$questions$proposals, function(q) {
      character()
    })
    names(proposal_cols) <- purrr::map_chr(
      form_meta$questions$proposals, ~ glue::glue("{.x$number}: {.x$title}")
    )
    return(tibble::tibble(
      timestamp = lubridate::as_datetime(character()),
      email = character(),
      `GitHub username (optional)` = character(),
      !!!proposal_cols
    ))
  }

  answer_text <- function(answers, question_id) {
    answer <- answers[[question_id]]
    if (is.null(answer)) {
      return(NA_character_)
    }
    answer$textAnswers$answers[[1]]$value
  }

  rows <- purrr::map(responses, function(r) {
    row <- tibble::tibble(
      timestamp = r$lastSubmittedTime,
      email = r$respondentEmail %||% NA_character_
    )
    row[["GitHub username (optional)"]] <-
      answer_text(r$answers, form_meta$questions$github_username)
    for (q in form_meta$questions$proposals) {
      col_name <- glue::glue("{q$number}: {q$title}")
      row[[col_name]] <- answer_text(r$answers, q$question_id)
    }
    row
  })

  dplyr::bind_rows(rows) |>
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
