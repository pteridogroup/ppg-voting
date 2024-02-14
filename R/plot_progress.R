library(tidyverse)

source("R/functions.R")

# Download list of issues (proposals)
issues <-
  fetch_issues("pteridogroup/ppg") %>%
  select(title, created_at) %>%
  filter(!str_detect(title, "\\[NOT VALID\\]")) %>%
  mutate(passed = str_detect(title, "\\[PASSED\\]")) %>%
  mutate(not_passed = str_detect(title, "\\[NOT PASSED\\]")) %>%
  mutate(voting = !str_detect(title, "PASSED")) %>%
  mutate(created_at = str_remove_all(created_at, "T.*Z") %>%
    lubridate::ymd())

issues_summary <-
  issues %>%
  mutate(month = lubridate::month(created_at) %>%
    str_pad(side = "left", pad = "0", width = 2)) %>%
  mutate(year = lubridate::year(created_at)) %>%
  mutate(year_month = paste(year, month, sep = "-")) %>%
  group_by(year_month) %>%
  summarize(
    passed = sum(passed),
    not_passed = sum(not_passed),
    under_vote = sum(voting)
  )

issues %>%
  filter(passed | not_passed) %>%
  mutate(month = lubridate::month(created_at) %>%
    str_pad(side = "left", pad = "0", width = 2)) %>%
  mutate(year = lubridate::year(created_at)) %>%
  mutate(year_month = paste(year, month, sep = "-")) %>%
  group_by(year_month) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(n_total = cumsum(n)) %>%
  mutate(dummy = "a") %>%
  ggplot(aes(x = year_month, y = n_total, group = dummy)) +
  geom_line() +
  geom_point()

plot <-
  issues_summary %>%
  pivot_longer(names_to = "type", values_to = "count", -year_month) %>%
  ggplot(aes(x = year_month, y = count, fill = type)) + geom_col(position = "dodge")

ggsave(plot = plot, file = "plot.png")
