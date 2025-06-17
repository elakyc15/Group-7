library(ggplot2)
library(dplyr)

# Combine 2017 and 2019 data with year column
rent_burden_2017 <- rent_burden_2017 %>% mutate(jaar = 2017)
rent_burden_2019 <- rent_burden_2019 %>% mutate(jaar = 2019)

rent_burden_clean <- bind_rows(rent_burden_2017, rent_burden_2019) %>%
  filter(!is.na(Rent_burden), !is.na(stadsdeel), !is.na(wijk))

ggplot(rent_burden_clean, aes(x = reorder(wijk, Rent_burden), y = Rent_burden, fill = stadsdeel)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.6) +  # thinner boxes to spread horizontally
  geom_jitter(aes(color = factor(jaar)), width = 0.2, size = 2, alpha = 0.7) +
  scale_fill_manual(values = c("Zuid" = "#1f78b4", "West" = "#33a02c")) +
  scale_color_manual(values = c("2017" = "#e41a1c", "2019" = "#377eb8")) +
  labs(
    title = "Rent Burden per Neighbourhood (2017 & 2019)",
    x = "Neighbourhood (Wijk)",
    y = "Rent Burden (% of income spent on rent)",
    fill = "Stadsdeel",
    color = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # improve x labels readability
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))  # add vertical space above boxes to make them appear taller

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


# --- multi line plot rent burden ---

library(ggplot2)

# Assuming your dataset is called df_merged or merged_rent_burden
ggplot(data_with_growth, aes(x = jaar, y = rent_burden, color = stadsdeel)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2015, 2017, 2019)) +
  labs(
    title = "Rent Burden in West and Zuid (2015–2019)",
    x = "Year",
    y = "Rent Burden (%)",
    color = "Stadsdeel"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)

base_2015 <- data.frame(
  stadsdeel = c("West", "Zuid"),
  jaar = 2015,
  affordability_index = 100
)

growth_data <- data_with_growth %>%
  filter(jaar %in% c(2017, 2019)) %>%
  group_by(stadsdeel) %>%
  mutate(
    affordability_index = 100 + affordability_pressure
  ) %>%
  select(stadsdeel, jaar, affordability_index)

plot_data <- bind_rows(base_2015, growth_data) %>%
  arrange(stadsdeel, jaar)

ggplot(plot_data, aes(x = jaar, y = affordability_index, color = stadsdeel)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  annotate("text", 
           x = 2018, 
           y = max(plot_data$affordability_index, na.rm = TRUE) * 0.98,  # just a little lower
           label = "Airbnb Cap", 
           angle = 90, 
           vjust = -0.5, 
           hjust = 1, 
           size = 4, 
           color = "black") +
  scale_x_continuous(breaks = c(2015, 2017, 2019)) +
  labs(
    title = "Affordability Pressure Index (2015 = 100)",
    x = "Year",
    y = "Affordability Index",
    color = "Stadsdeel"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

