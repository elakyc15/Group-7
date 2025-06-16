# Load necessary libraries
library(ggplot2)
library(dplyr)

# Make sure your data is clean
rent_burden_clean <- rent_burden_2017 %>%
  filter(!is.na(Rent_burden), !is.na(stadsdeel), !is.na(wijk))

# Create the boxplot without skew highlighting
ggplot(rent_burden_clean, aes(x = reorder(wijk, Rent_burden), y = Rent_burden, fill = stadsdeel)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +  # suppress outliers for cleaner plot
  geom_jitter(aes(color = stadsdeel), width = 0.2, size = 3, alpha = 0.8) +  # show points
  scale_fill_manual(values = c("Zuid" = "#1f78b4", "West" = "#33a02c")) +
  scale_color_manual(values = c("Zuid" = "#1f78b4", "West" = "#33a02c")) +
  labs(
    title = "Rent Burden per Neighbourhood (2017)",
    x = "Neighbourhood (Wijk)",
    y = "Rent Burden (% of income spent on rent)",
    fill = "Stadsdeel",
    color = "Stadsdeel"
  ) +
  coord_flip() +  # Flip for readability
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )



# --- Map Visualization: average rent burden per wijk ---

# Assuming amsterdam_map is an sf object with 'wijk' column to join on
# Calculate mean rent burden per wijk
mean_rent_map <- rent_burden_clean %>%
  group_by(wijk) %>%
  summarise(mean_rent_burden = mean(Rent_burden, na.rm = TRUE))

# Join spatial data with rent burden data
map_data <- amsterdam_map %>%
  left_join(mean_rent_map, by = "wijk")

# Plot map showing where rent pressure is most severe
ggplot(map_data) +
  geom_sf(aes(fill = mean_rent_burden), color = "white", size = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90", direction = -1) +
  labs(
    title = "Average Rent Burden by Neighbourhood in Amsterdam",
    fill = "Mean Rent Burden (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )
