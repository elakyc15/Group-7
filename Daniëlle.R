# Make sure the datasets have the correct names
# Assuming huur_long contains rent data and income data is in inkomen_2015, 2017, and 2019

# Combine income data for all years
inkomen <- bind_rows(inkomen_2015, inkomen_2017, inkomen_2019)

# Merge income and rent data
merged_df <- inkomen %>%
  left_join(huur_long, by = c("stadsdeel", "jaar"))

# Calculate monthly income and rent burden (huur_quota)
merged_df$gem_inkomen <- (merged_df$gem_inkomen * 1000) / 12  # Convert income to monthly
merged_df$huur_quota <- (merged_df$huurprijs / merged_df$gem_inkomen) * 100  # Rent burden as percentage

# Calculate growth in income and rent prices for affordability pressure
data_with_growth <- merged_df %>%
  arrange(stadsdeel, jaar) %>%
  group_by(stadsdeel) %>%
  mutate(
    groei_inkomen = (gem_inkomen - lag(gem_inkomen)) / lag(gem_inkomen) * 100,
    groei_huurprijs = (huurprijs - lag(huurprijs)) / lag(huurprijs) * 100
  )
# Affordability pressure as the difference between rent price growth and income growth
data_with_growth$affordability_pressure <- (data_with_growth$groei_huurprijs - data_with_growth$groei_inkomen)

# Read Amsterdam shapefile (GeoJSON)
amsterdam_map <- st_read("data/adam.geojson")

# Filter to include only Amsterdam neighbourhoods
amsterdam_map <- amsterdam_map %>%
  rename(wijk = wijknaam) %>%
  filter(gemeentenaam == "Amsterdam")

# Merge neighbourhood map data with rent and income data for Zuid and West
combined_df <- bind_rows(data_zuid_2017, data_west_2017)
map_data <- amsterdam_map %>%
  left_join(combined_df, by = "wijk")

# Plot the rent burden (huur quota) on the map
ggplot(data = map_data) +
  geom_sf(aes(fill = huur_quota), color = "white") +  # Fill based on rent burden
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +  # Custom colour scale
  theme_minimal() +
  labs(title = "Rent Burden (Huur Quota) in Amsterdam Zuid and West", 
       fill = "Rent Burden (%)")  # Rent burden in percentage