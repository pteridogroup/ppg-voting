library(jsonlite)
library(tidyverse)

# Import issues from JSON
issues_json <- fromJSON(
  "https://api.github.com/repos/pteridogroup/ppg/issues?state=all")

# Extract useful information to dataframe
issues <-
  tibble(
    number = issues_json$number,
    title = issues_json$title,
    url = issues_json$url,
    created_at = issues_json$created_at,
    user = issues_json$user$login,
    state = issues_json$state,
    body = issues_json$body #,
    # draft = issues_json$draft # draft only applies to PRs, use if PRs exist
  ) %>%
    # Remove PRs: issues have NA for draft
    # filter(is.na(draft)) %>%
    # select(-draft) %>%
  mutate(
    url = str_replace_all(
      url, "https://api.github.com/repos/", "https://github.com/"),
    name = str_match(body, "Name of taxon\n\n(.*)") %>%
             magrittr::extract(, 2),
    rank = str_match(body, "Rank of taxon\n\n(.*)") %>%
             magrittr::extract(, 2),
    no_species = str_match(body, "number of species affected\n\n(.*)") %>%
             magrittr::extract(, 2)
  ) %>%
    select(-body)