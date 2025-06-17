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

