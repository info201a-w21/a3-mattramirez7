# Overview ----------------------------------------------------------------

# Assignment 3: Incarceration


# Loading data ------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(maps)
library(ggplot2)

incarceration <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# Summary Info ------------------------------------------------------------

county_high_blk_jail_pop <- incarceration %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(county_name)

state_high_blk_jail_pop <- incarceration %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(state)

county_high_wht_jail_pop <- incarceration %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
  pull(county_name)

year_high_blk_jail_pop <- incarceration %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(year)

state_high_wht_jail_pop <- incarceration %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
  pull(state)

# Trends Over Time Chart --------------------------------------------------

jail_pop_rate <- incarceration %>%
  filter(county_name == "Los Angeles County") %>%
  filter(year > 1989) %>%
  select(
    year, black_jail_pop_rate, aapi_jail_pop_rate, white_jail_pop_rate,
    native_jail_pop_rate, latinx_jail_pop_rate
  )

jail_chart <- jail_pop_rate %>%
  pivot_longer(
    cols = c(
      "black_jail_pop_rate", "aapi_jail_pop_rate", "white_jail_pop_rate",
      "native_jail_pop_rate", "latinx_jail_pop_rate"
    ),
    names_to = "race",
    values_to = "value"
  )

jail_pop_rate_over_time <- jail_chart %>%
  ggplot() +
  geom_point(aes(x = year, y = value, group = race, color = race)) +
  labs(
    x = "Year", y = "Jail Population Rate",
    title = "Jail Population Rate Over Time in Los Angeles County"
  )



# Variable Comparison Chart -----------------------------------------------

los_angeles_data <- incarceration %>%
  filter(county_name == "Los Angeles County") %>%
  filter(year > 1984)

los_angeles_blk_jail_prop <- los_angeles_data %>%
  ggplot() +
  geom_point(aes(x = total_jail_pop, y = black_jail_pop)) +
  labs(
    x = "Total Jail Population", y = "Black Jail Population",
    title = "Los Angeles County Black Jail Proportion"
  )

# Map ---------------------------------------------------------------------

# Setting Up Data

recent_data <- incarceration %>%
  filter(year == max(year))

# Joining Data Sets

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(recent_data, by = "fips") %>%
  filter(state == "WA", county_name != "Unknown")

# Create Blank Theme

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Creating the Map

black_jail_pop_rate_map_wa <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_jail_pop_rate)),
    na.value = "white", low = "yellow", high = "red"
  ) +
  blank_theme +
  ggtitle("Black Jail Population Rate in Washington")
