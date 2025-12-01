# Tabulate types of changes made so far relative to PPG I

# does not account for all proposals yet

library(tidyverse)
source("R/functions.R")

issues <- fetch_issues("pteridogroup/ppg")

issues |>
  filter(str_detect(title, "\\[PASSED\\]")) |>
  mutate(
    name = str_remove_all(name, regex("and|,", ignore_case = TRUE)) |>
      str_squish()
  ) |>
  select(number, name, rank, description) |>
  mutate(
    change = case_when(
      str_detect(description, "should be sunk") ~ "sink",
      str_detect(description, "subsumed") ~ "sink",
      str_detect(description, fixed("recogni", ignore_case = TRUE)) ~ "split",
      str_detect(description, fixed("Resurrect", ignore_case = TRUE)) ~ "split",
      str_detect(description, "to be a distinct group") ~ "split",
      str_detect(description, "accommodate") ~ "split",
      str_detect(description, "segregate") ~ "split",
      str_detect(description, "into synonymy") ~ "sink",
      str_detect(description, "transferred to") ~ "sink",
      str_detect(description, "accommodate") ~ "split",
      str_detect(description, "lump") ~ "sink",
      .default = NA_character_
    )
  ) |>
  separate_rows(name) |>
  filter(rank != "Genus") |>
  write_csv("~/Desktop/change_other.csv")

readr::read_csv("~/Desktop/changes.csv") |>
  filter(exclude == 0) |>
  filter(change != "NA") |>
  count(change)

readr::read_csv("~/Desktop/changes_edited.csv") |>
  filter(change != "none") |>
  mutate(
    rank = factor(
      rank,
      levels = c("Family", "Subfamily", "Genus", "Nothogenus")
    )
  ) |>
  count(rank, change) |>
  mutate(
    effect = case_when(
      change %in% c("conserve", "expand", "shrink", "transfer") ~ "none",
      change == "sink" ~ "decrease",
      change == "split" ~ "increase",
    )
  ) |>
  filter(
    change %in% c("conserve", "sink", "split", "transfer")
  ) |>
  mutate(
    change = str_to_sentence(change),
    effect = str_to_sentence(effect)
  ) |>
  arrange(rank, change) |>
  select(
    Rank = rank,
    Change = change,
    Effect = effect,
    n = n
  )
