# install.packages(c("sf", "ggplot2", "dplyr", "readr"))
# # Optional for interactive map:
# install.packages("leaflet")

library(sf)
library(dplyr)
library(ggplot2)

inkomen <- bind_rows(inkomen_2015, inkomen_2017, inkomen_2019)



merged_df <- inkomen %>%
  left_join(huur_long, by = c("stadsdeel", "jaar"))
merged_df$gem_inkomen <- (merged_df$gem_inkomen * 1000) / 12
merged_df$huur_quota <- (merged_df$huurprijs / merged_df$gem_inkomen) * 100
merged_df$affordability_pressure <- (merged_df$huurprijs / merged_df$gem_inkomen) * 100


amsterdam_map <- st_read("data/adam.geojson")

amsterdam_map <- amsterdam_map %>%
  rename(wijk = wijknaam)
amsterdam_map <- amsterdam_map %>%
  filter(gemeentenaam == "Amsterdam")

combined_df <- bind_rows(data_zuid_2017, data_west_2017)
map_data <- amsterdam_map %>%
  left_join(combined_df, by = "wijk")

# Plot
ggplot(data = map_data) +
  geom_sf(aes(fill = gem_inkomen), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Gemiddeld Inkomen per Wijk in Amsterdam Zuid en West",
       fill = "Gem. Inkomen")


