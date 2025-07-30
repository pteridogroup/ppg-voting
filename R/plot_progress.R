library(tidyverse)

source("R/functions.R")

# set color palette
okabe_ito_cols <- c(
  orange = "#E69F00",
  skyblue = "#56B4E9",
  bluishgreen = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  vermillion = "#D55E00",
  reddishpurple = "#CC79A7"
)

# Download list of issues (proposals)
issues <-
  fetch_issues("pteridogroup/ppg") %>%
  select(title, name, created_at) %>%
  filter(!str_detect(title, "\\[NOT VALID\\]")) %>%
  mutate(passed = str_detect(title, "\\[PASSED\\]")) %>%
  mutate(not_passed = str_detect(title, "\\[NOT PASSED\\]")) %>%
  mutate(voting = !str_detect(title, "PASSED")) %>%
  mutate(
    created_at = str_remove_all(created_at, "T.*Z") %>%
      lubridate::ymd()
  ) %>%
  mutate(
    month = lubridate::month(created_at) %>%
      str_pad(side = "left", pad = "0", width = 2)
  ) %>%
  mutate(year = lubridate::year(created_at)) %>%
  mutate(year_month = paste(year, month, sep = "-"))

issues_summary <-
  issues %>%
  group_by(year_month) %>%
  summarize(
    passed = sum(passed),
    not_passed = sum(not_passed),
    under_vote = sum(voting)
  )

# Make plot of progress *by name*
issues %>%
  mutate(
    name = str_replace_all(name, "and", ",") %>%
      str_squish()
  ) %>%
  separate_rows(name, sep = ",") %>%
  mutate(name = str_squish(name)) %>%
  pull(name)

# Make plot of cumulative count of submitted proposals
cum_issue_plot <-
  issues %>%
  group_by(year_month) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(n_total = cumsum(n)) %>%
  mutate(dummy = "a") %>%
  ggplot(aes(x = year_month, y = n_total, group = dummy)) +
  # geom_hline(yintercept = 80, linetype = "dashed", color = "grey30") +
  geom_line() +
  geom_point(size = 3) +
  # annotate(
  #   "label",
  #   x = "2024-02",
  #   y = 80,
  #   label = "Goal",
  #   size = 10,
  #   color = "darkred") +
  labs(
    title = "Cumulative count of proposals submitted to PPG",
    y = "Number of proposals"
  ) +
  theme_gray(base_size = 22) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

ggsave(plot = cum_issue_plot, file = "results/cum_issue_plot.png")

# Check total number of passed/not passed
issues_summary %>%
  pivot_longer(names_to = "type", values_to = "count", -year_month) %>%
  group_by(type) %>%
  summarize(count = sum(count))

vote_res_plot <-
  issues_summary %>%
  pivot_longer(names_to = "type", values_to = "count", -year_month) %>%
  mutate(year_month = lubridate::ym(year_month)) %>%
  ggplot(aes(x = year_month, y = count, fill = type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Results of proposals submitted to PPG",
    y = "Number of proposals"
  ) +
  scale_fill_manual(
    values = c(
      "passed" = okabe_ito_cols[["bluishgreen"]],
      "not_passed" = okabe_ito_cols[["vermillion"]],
      "under_vote" = okabe_ito_cols[["skyblue"]]
    ),
    breaks = c("passed", "not_passed", "under_vote"),
    labels = c("Passed", "Not passed", "TBD")
  ) +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "4 months"
  ) +
  theme_gray(base_size = 22) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

ggsave(
  plot = vote_res_plot,
  file = "results/vote_res_plot.png",
  width = 8,
  height = 7
)
