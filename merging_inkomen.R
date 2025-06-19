# Load required libraries
library(sf)       
library(dplyr)    
library(ggplot2)  
# Load required package
library(dplyr)

# Load rental price data, skipping the first row which likely contains metadata
huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)

# Keep only rows for West (E West) and Zuid (K Zuid)
huur <- huur[huur$V2 %in% c("E West", "K Zuid"), ]

# Rename columns for clarity
colnames(huur) <- c("Index", "stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p", 
                    "2013_c", "2015_c", "2017_c", "2019_c")

# Select only private sector rent columns and district names
huur_particulier <- huur[, c("stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p")]

# Recode district names to simplified labels
huur_particulier <- huur_particulier %>%
  mutate(stadsdeel = recode(stadsdeel, "E West" = "West", "K Zuid" = "Zuid"))

# Remove '_p' suffix from column names
colnames(huur_particulier) <- sub("_p", "", colnames(huur_particulier)) 

# Convert year columns to numeric
huur_particulier[, -1] <- lapply(huur_particulier[, -1], as.numeric)

# Reshape data to long format: one row per district-year with rental price
huur_long <- pivot_longer(huur_particulier, 
                          cols = -stadsdeel, 
                          names_to = "jaar", 
                          values_to = "huurprijs")
huur_long$jaar <- as.numeric(huur_long$jaar)

# Load income data for 2015
inkomen_2015 <- read_csv("data/inkomen_2015.csv")

# Combine income data across multiple years into a single dataframe
inkomen <- bind_rows(inkomen_2015, inkomen_2017, inkomen_2019)

# Convert 'jaar' columns in both dataframes to character
inkomen$jaar <- as.character(inkomen$jaar)
huur_long$jaar <- as.character(huur_long$jaar)

# Merge inkomen with huur_long by 'stadsdeel' and 'jaar'
merged_df <- inkomen %>%
  left_join(huur_long, by = c("stadsdeel", "jaar"))


# Convert average annual income (in thousands of euros) to monthly income in euros
merged_df$gem_inkomen <- (merged_df$gem_inkomen * 1000) / 12

# Calculate rent burden as a percentage of monthly income
merged_df$rent_burden <- (merged_df$huurprijs / merged_df$gem_inkomen) * 100

# Arrange the data by district (stadsdeel) and year for proper chronological order
data_with_growth <- merged_df %>%
  arrange(stadsdeel, jaar) %>%
  group_by(stadsdeel) %>%
  mutate(
    # Calculate income growth (%) compared to the previous year within each district
    groei_inkomen = (gem_inkomen - lag(gem_inkomen)) / lag(gem_inkomen) * 100,
    
    # Calculate rent price growth (%) compared to the previous year within each district
    groei_huurprijs = (huurprijs - lag(huurprijs)) / lag(huurprijs) * 100
  )

# Compute affordability pressure as the difference between rent growth and income growth
data_with_growth$affordability_pressure <- (data_with_growth$groei_huurprijs - data_with_growth$groei_inkomen)


# Merge rent burden data for 2015, 2017, and 2019 into a single dataframe
merged_rent_burden <- bind_rows(rent_burden_2015, rent_burden_2017, rent_burden_2019)

# Add a 'jaar' (year) column to each individual year's dataset
rent_burden_2015 <- rent_burden_2015 %>% mutate(jaar = 2015)
rent_burden_2017 <- rent_burden_2017 %>% mutate(jaar = 2017)
rent_burden_2019 <- rent_burden_2019 %>% mutate(jaar = 2019)

# Recombine the updated datasets with year included and sort by year
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




#write_csv(avg_west, "data/avg_west.csv")
#write_csv(avg_zuid, "data/avg_zuid.csv")
#write_csv(combined_df, "data/combined_df.csv")
#write_csv(data_west, "data/data_west.csv")
#write_csv(data_zuid, "data/data_zuid.csv")
#write_csv(data_with_growth, "data_with_growtht.csv")
#write_csv(inkomen, "inkomen.csv")
#write_csv(merged_rent_burden, "merged_rent_burden.csv")
