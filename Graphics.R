# --- Boxplot of Rent Burden per Neighbourhood (Wijk) for 2017 and 2019 ---

library(ggplot2)
library(dplyr)

# Add year column to each dataset
rent_burden_2017 <- rent_burden_2017 %>% mutate(jaar = 2017)
rent_burden_2019 <- rent_burden_2019 %>% mutate(jaar = 2019)

# Combine data from both years and remove incomplete entries
rent_burden_clean <- bind_rows(rent_burden_2017, rent_burden_2019) %>%
  filter(!is.na(Rent_burden), !is.na(stadsdeel), !is.na(wijk))

# Create the boxplot
ggplot(rent_burden_clean, aes(x = reorder(wijk, Rent_burden), y = Rent_burden, fill = stadsdeel)) +
  
  # Draw boxplots without outliers and semi-transparent fill
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.6) +
  
  # Add jittered points to show individual data by year
  geom_jitter(aes(color = factor(jaar)), width = 0.2, size = 2, alpha = 0.7) +
  
  # Manually set colors for districts (stadsdelen)
  scale_fill_manual(values = c("Zuid" = "#1f78b4", "West" = "#33a02c")) +
  
  # Manually set colors for years
  scale_color_manual(values = c("2017" = "#e41a1c", "2019" = "#377eb8")) +
  
  # Labels and title
  labs(
    title = "Rent Burden Distribution Across Neighbourhoods in West and Zuid (2017 & 2019)",
    x = "Neighbourhood (Wijk)",
    y = "Rent Burden (% of income spent on rent)",
    fill = "Stadsdeel",
    color = "Year"
  ) +
  
  # Apply a clean minimal theme
  theme_minimal(base_size = 14) +
  
  # Improve x-axis text readability and title style
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  
  # Add space above boxplots to avoid clipping
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))


library(dplyr)
library(ggplot2)
library(viridis)

# --- Choropleth Maps: Rent Burden by Neighbourhood (2017 & 2019) ---

# --- Prepare 2017 Data ---

amsterdam_map <- st_read("data/adam.geojson")
amsterdam_map <- amsterdam_map %>%
  rename(wijk = wijknaam)
amsterdam_map <- amsterdam_map %>%
  filter(gemeentenaam == "Amsterdam")

combined_df <- bind_rows(data_zuid_2017, data_west_2017)
map_data <- amsterdam_map %>%
  left_join(combined_df, by = "wijk")

# Compute mean rent burden per neighbourhood (wijk) for 2017
mean_rent_map_2017 <- rent_burden_2017 %>%
  filter(!is.na(Rent_burden)) %>%
  group_by(wijk) %>%
  summarise(mean_rent_burden = mean(Rent_burden, na.rm = TRUE))

# Join mean rent burden to spatial map data for 2017
map_data_2017 <- amsterdam_map %>%
  left_join(mean_rent_map_2017, by = "wijk")

# --- Prepare 2019 Data ---

# Compute mean rent burden per neighbourhood (wijk) for 2019
mean_rent_map_2019 <- rent_burden_2019 %>%
  filter(!is.na(Rent_burden)) %>%
  group_by(wijk) %>%
  summarise(mean_rent_burden = mean(Rent_burden, na.rm = TRUE))

# Join mean rent burden to spatial map data for 2019
map_data_2019 <- amsterdam_map %>%
  left_join(mean_rent_map_2019, by = "wijk")

# --- Define Shared Color Scale Across Both Years ---

# Determine combined min/max range to ensure consistent fill scale
combined_range <- range(
  c(mean_rent_map_2017$mean_rent_burden, mean_rent_map_2019$mean_rent_burden),
  na.rm = TRUE
)

# Generate breaks every 5% for legend ticks
breaks_seq <- seq(
  floor(combined_range[1] / 5) * 5,
  ceiling(combined_range[2] / 5) * 5,
  by = 5
)

# --- Plot: Average Rent Burden by Neighbourhood in 2017 ---

ggplot(map_data_2017) +
  geom_sf(aes(fill = mean_rent_burden), color = "white", size = 0.3) +  # draw neighbourhoods
  scale_fill_viridis_c(   # apply perceptually uniform color scale
    option = "magma",
    na.value = "grey90",   # color for NA values
    direction = -1,        # reverse scale direction
    limits = combined_range,  # consistent scale across years
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

# --- Plot: Average Rent Burden by Neighbourhood in 2019 ---

ggplot(map_data_2019) +
  geom_sf(aes(fill = mean_rent_burden), color = "white", size = 0.3) +  # draw neighbourhoods
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



# --- Multi-line plot of Rent Burden over time for West and Zuid ---

library(ggplot2)

# Create a line plot showing the rent burden across years for each stadsdeel (district)
ggplot(data_with_growth, aes(x = jaar, y = rent_burden, color = stadsdeel)) +
  
  # Add lines to connect rent burden values over time for each district
  geom_line(linewidth = 1.2) +
  
  # Add points at each data value for better visibility
  geom_point(size = 2) +
  
  # Set specific breaks on the x-axis (only the measured years)
  scale_x_continuous(breaks = c(2015, 2017, 2019)) +
  
  # Add plot labels and title
  labs(
    title = "Rent Burden in West and Zuid (2015–2019)",  
    x = "Year",                                           
    y = "Rent Burden (%)",                                
    color = "Stadsdeel"                                   
  ) +
  
  # Apply a minimal theme for a clean appearance
  theme_minimal()



library(dplyr)
library(ggplot2)

# --- Line Plot of Affordability Pressure Index (2015 = 100) with Airbnb Cap Annotation ---

# Create base values for 2015: index set to 100 for both stadsdelen
base_2015 <- data.frame(
  stadsdeel = c("West", "Zuid"),
  jaar = 2015,
  affordability_index = 100
)

# Prepare affordability index for 2017 and 2019
growth_data <- data_with_growth %>%
  filter(jaar %in% c(2017, 2019)) %>%         # Only take years after 2015
  group_by(stadsdeel) %>%
  mutate(
    # Affordability index is calculated by adding affordability pressure to the 2015 baseline (100)
    affordability_index = 100 + affordability_pressure
  ) %>%
  select(stadsdeel, jaar, affordability_index)

# Combine baseline with growth data and sort
plot_data <- bind_rows(base_2015, growth_data) %>%
  arrange(stadsdeel, jaar)

# Create the line plot
ggplot(plot_data, aes(x = jaar, y = affordability_index, color = stadsdeel)) +
  
  # Draw trend lines for each stadsdeel
  geom_line(linewidth = 1.3) +
  
  # Add data points
  geom_point(size = 3) +
  
  # Add a dashed vertical line for the policy intervention (Airbnb cap)
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  
  # Annotate the Airbnb Cap line
  annotate("text", 
           x = 2018, 
           y = max(plot_data$affordability_index, na.rm = TRUE) * 0.98,  # slightly below the top
           label = "Airbnb Cap", 
           angle = 90, 
           vjust = -0.5, 
           hjust = 1, 
           size = 4, 
           color = "black") +
  
  # Define x-axis breaks
  scale_x_continuous(breaks = c(2015, 2017, 2019)) +
  
  # Labels and title
  labs(
    title = "Affordability Pressure Index (2015 = 100)",
    x = "Year",
    y = "Affordability Index",
    color = "Stadsdeel"
  ) +
  
  # Clean, minimal theme with bold title
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )
