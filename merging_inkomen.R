# install.packages(c("sf", "ggplot2", "dplyr", "readr"))
# # Optional for interactive map:
# install.packages("leaflet")

library(sf)
library(dplyr)
library(ggplot2)
inkomen_2015 <- read_csv("data/inkomen_2015.csv")
inkomen <- bind_rows(inkomen_2015, inkomen_2017, inkomen_2019)


merged_df <- inkomen %>%
  left_join(huur_long, by = c("stadsdeel", "jaar"))
merged_df$gem_inkomen <- (merged_df$gem_inkomen * 1000) / 12
merged_df$rent_burden <- (merged_df$huurprijs / merged_df$gem_inkomen) * 100


data_with_growth <- merged_df %>%
  arrange(stadsdeel, jaar) %>%
  group_by(stadsdeel) %>%
  mutate(
    groei_inkomen = (gem_inkomen - lag(gem_inkomen)) / lag(gem_inkomen) * 100,
    groei_huurprijs = (huurprijs - lag(huurprijs)) / lag(huurprijs) * 100
  )
data_with_growth$affordability_pressure <- (data_with_growth$groei_huurprijs - data_with_growth$groei_inkomen)

#merge rentburden 2015, 2017, 2019
merged_rent_burden <- bind_rows(rent_burden_2015, rent_burden_2017, rent_burden_2019)
rent_burden_2015 <- rent_burden_2015 %>% mutate(jaar = 2015)
rent_burden_2017 <- rent_burden_2017 %>% mutate(jaar = 2017)
rent_burden_2019 <- rent_burden_2019 %>% mutate(jaar = 2019)

merged_rent_burden <- bind_rows(rent_burden_2015, rent_burden_2017, rent_burden_2019) %>%
  arrange(jaar)



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

#multi line plot rent burden
#library(ggplot2)

# Assuming your dataset is called df_merged or merged_rent_burden
#ggplot(data_with_growth, aes(x = jaar, y = rent_burden, color = stadsdeel)) +
  #geom_line(linewidth = 1.2) +
  #geom_point(size = 2) +
  #scale_x_continuous(breaks = c(2015, 2017, 2019)) +
  #labs(
    #title = "Rent Burden in West and Zuid (2015–2019)",
    #x = "Year",
    #y = "Rent Burden (%)",
    #color = "Stadsdeel"
  #) +
  #theme_minimal()




write_csv(avg_west, "data/avg_west.csv")
write_csv(avg_zuid, "data/avg_zuid.csv")
write_csv(combined_df, "data/combined_df.csv")
write_csv(data_west, "data/data_west.csv")
write_csv(data_zuid, "data/data_zuid.csv")
write_csv(data_with_growth, "data_with_growtht.csv")
write_csv(inkomen, "inkomen.csv")
write_csv(merged_rent_burden, "merged_rent_burden.csv")
