library(dplyr)
library(ggplot2)
library(viridis)

# Prepare data for 2017
mean_rent_map_2017 <- rent_burden_2017 %>%
  filter(!is.na(Rent_burden)) %>%
  group_by(wijk) %>%
  summarise(mean_rent_burden = mean(Rent_burden, na.rm = TRUE))

map_data_2017 <- amsterdam_map %>%
  left_join(mean_rent_map_2017, by = "wijk")

# Prepare data for 2019
mean_rent_map_2019 <- rent_burden_2019 %>%
  filter(!is.na(Rent_burden)) %>%
  group_by(wijk) %>%
  summarise(mean_rent_burden = mean(Rent_burden, na.rm = TRUE))

map_data_2019 <- amsterdam_map %>%
  left_join(mean_rent_map_2019, by = "wijk")

# Define combined range across both years for consistent color scale
combined_range <- range(
  c(mean_rent_map_2017$mean_rent_burden, mean_rent_map_2019$mean_rent_burden),
  na.rm = TRUE
)

# Define breaks every 5 units for legend ticks
breaks_seq <- seq(
  floor(combined_range[1] / 5) * 5,
  ceiling(combined_range[2] / 5) * 5,
  by = 5
)

# Plot 2017 map
ggplot(map_data_2017) +
  geom_sf(aes(fill = mean_rent_burden), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    direction = -1,
    limits = combined_range,
    breaks = breaks_seq
  ) +
  labs(
    title = "Average Rent Burden by Neighbourhood in Amsterdam (2017)",
    fill = "Mean Rent Burden (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )

# Plot 2019 map
ggplot(map_data_2019) +
  geom_sf(aes(fill = mean_rent_burden), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    direction = -1,
    limits = combined_range,
    breaks = breaks_seq
  ) +
  labs(
    title = "Average Rent Burden by Neighbourhood in Amsterdam (2019)",
    fill = "Mean Rent Burden (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )

# Load libraries
library(ggplot2)
library(dplyr)
library(readr)

# Combine income data
inkomen_2015 <- read_csv("data/inkomen_2015.csv")
inkomen <- bind_rows(inkomen_2015, inkomen_2017, inkomen_2019)

# Preprocessing: combine with rent data and calculate rent burden
merged_df <- inkomen %>%
  left_join(huur_long, by = c("stadsdeel", "jaar")) %>%
  mutate(
    gem_inkomen = (gem_inkomen * 1000) / 12,
    rent_burden = (huurprijs / gem_inkomen) * 100
  )

library(ggplot2)
library(dplyr)

# --- Step 1: Create baseline values (2015 = 100) per stadsdeel
baseline <- data_with_growth %>%
  filter(jaar == 2015) %>%
  select(stadsdeel, baseline = affordability_pressure)

# --- Step 2: Join baseline and compute index
plot_data <- data_with_growth %>%
  left_join(baseline, by = "stadsdeel") %>%
  mutate(affordability_index = ifelse(!is.na(baseline) & baseline != 0, (affordability_pressure / baseline) * 100, NA))

# --- Step 3: Create the line plot
ggplot(plot_data, aes(x = jaar, y = affordability_index, color = stadsdeel)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019)) +
  labs(
    title = "Affordability Pressure Over Time (2015 = 100)",
    x = "Year",
    y = "Affordability Index",
    color = "Stadsdeel"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

