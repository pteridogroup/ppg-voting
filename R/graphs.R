library(tidyverse)
library(lubridate)

source("R/functions.R")

# Get dataframe of all isssues
issues <- fetch_issues("pteridogroup/ppg") |>
  filter(str_detect(title, "\\[NOT VALID\\]", negate = TRUE)) |>
  # FIXME remove when Sticheropsis issue is closed
  filter(str_detect(title, "Sticheropsis", negate = TRUE)) |>
  mutate(status = case_when(
    str_detect(title, "\\[PASSED\\]") ~ "Passed",
    str_detect(title, "\\[NOT PASSED\\]") ~ "Not passed",
    .default = "TBD"
  )) |>
  mutate(
    # Convert created_at to the 1st day of the month
    created_at = ymd_hms(created_at, tz = "UTC") |>
      floor_date(unit = "months") |> as.Date()
  ) |>
  select(number, status, created_at)

# Mean number of issues per month ----
mean_per_month <- issues |>
  count(created_at) |>
  summarize(n = mean(n)) |>
  mutate(
    mean_text = "Mean",
    created_at = floor_date(Sys.Date(), unit = "months")
  )

# Set overall goal for number of proposals
overall_goal <- 80

# Calculate time to reach goal from start (months)
overall_goal / mean_per_month$n

# Cumulative number of issues submitted ----

# define dataframe with goal
df_goal <- tibble(created_at = floor_date(Sys.Date(), unit = "months")) |>
  mutate(cum_count = overall_goal, goal_text = "Goal")

# Make initial dataframe of cumulative count
# but this lacks observations for months with zero proposals
cum_issues <-
  issues |>
  count(created_at) |>
  arrange(created_at) |>
  mutate(cum_count = cumsum(n))

# Make empty df with all dates
full_dates <- tibble(
  created_at = seq(
    min(cum_issues$created_at), max(cum_issues$created_at), by = "month")
)

# Join with the original data and fill missing values with 0
cum_issues_complete <- full_dates %>%
  left_join(cum_issues, by = "created_at") %>%
  replace_na(list(n = 0, cum_count = 0))

# Calculate the cumulative count again
cum_issues_complete <- cum_issues_complete %>%
  mutate(cum_count = cumsum(n))

ggplot(mapping = aes(x = created_at, y = cum_count)) +
  geom_line(data = cum_issues_complete, group = 1) +
  geom_point(data = cum_issues_complete) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(
    title = "Cumulative count of proposals submitted to PPG",
    y = "Number of proposals"
  ) +
  theme_gray(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

ggsave(
  "results/proposal_cumulative_count_plot.png",
  height = 16, width = 16, units = "cm"
)

# Results of submitted proposals ---

# define Okabe-Ito color palette
cols <- palette.colors(palette = "Okabe-Ito", names = TRUE)

results_per_month <- issues |>
  mutate(created_at = floor_date(created_at, unit = "months") |> as.Date()) |>
  group_by(created_at) |>
  count(status) |>
  ungroup()

ggplot() +
  geom_hline(
    data = mean_per_month,
    mapping = aes(yintercept = n),
    linetype = 2
  ) +
  geom_col(
    data = results_per_month,
    mapping = aes(x = created_at, y = n, fill = status)
  ) +
  geom_label(
    data = mean_per_month,
    mapping = aes(x = created_at, y = n, label = mean_text),
    size = 4, color = "grey20",
    hjust = 1
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 month") +
  scale_fill_manual(
    name = "Result",
    values = c(
      "Passed" = cols[["bluishgreen"]],
      "Not passed" = cols[["vermillion"]],
      "TBD" = cols[["skyblue"]]
    )
  ) +
  labs(
    title = "Results of proposals submitted to PPG",
    y = "Number of proposals"
  ) +
  theme_gray(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

ggsave(
  "results/proposal_results_plot.png",
  height = 16, width = 16, units = "cm"
)
