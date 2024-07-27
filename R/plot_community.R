library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)
source("R/functions.R")

email_file = "https://docs.google.com/spreadsheets/d/1vxlmf8QPndiE6dIeDcjoFT7GA3ZE4pSc_Z1raf4iuwA/edit?usp=sharing"

ppg_emails <- googlesheets4::read_sheet(email_file) |>
    janitor::clean_names()

ppg_com <- 
  ppg_emails %>%
  mutate(
    country = str_replace_all(country, "USA", "United States of America") %>%
      str_replace_all("UK", "United Kingdom") %>%
      str_replace_all("Brunei Darussalam", "Brunei")
    )

# Count participants by country
ppg_count <- ppg_com %>%
  count(country) %>%
  filter(!is.na(country))

# Get number of participants in Japan
n_india <- ppg_com %>%
  filter(country == "India") %>%
  nrow()

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# So centroid calculations work
sf_use_s2(FALSE)

# Calculate centroids
centroids <-
world %>%
  filter(name_sort %in% ppg_com$country) %>%
  st_centroid() %>%
  left_join(ppg_count, by = c(name_sort = "country"))

my_breaks <- c(5, 15, 30, 45)

ggplot() +
  geom_sf(data = world, fill = "transparent", color = "grey50") +
  geom_point(
    data = centroids,
    aes(size = n, fill = n, geometry = geometry),
    shape = 21,
    stat = "sf_coordinates") +
  scale_size_continuous(
    name = "Num. people",
    breaks = my_breaks,
    labels = my_breaks
  ) +
  scale_fill_viridis(
    option = "viridis",
    name = "Num. people",
    breaks = my_breaks,
    labels = my_breaks
  ) +
  guides(fill = guide_legend(), size = guide_legend())  +
  theme_gray(
    base_size = 18 ) +
  theme(
    plot.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    axis.title = element_blank(),
    legend.key = element_rect(fill = "transparent")
  )

  ggsave(file = "results/world_map.png")
