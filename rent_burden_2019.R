# Load required libraries
library(sf)        
library(dplyr)     
library(ggplot2)   

# Combine income data from West and Zuid neighborhoods for 2019
inkomen_per_wijk_2019 <- bind_rows(data_west_2019, data_zuid_2019)

# Duplicate the data for calculating rent burden
rent_burden_2019 <- inkomen_per_wijk_2019

# Calculate average monthly income from annual income (in euros, originally in thousands)
rent_burden_2019 <- rent_burden_2019 %>%
  mutate(gem_inkomen_per_maand = gem_inkomen * 1000 / 12)

# Define neighborhoods belonging to Zuid
wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')

# Define neighborhoods belonging to West
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

# Initialize the 'stadsdeel' (district) column
rent_burden_2019$stadsdeel <- 0

# Assign 'Zuid' to neighborhoods in Zuid
rent_burden_2019$stadsdeel <- rent_burden_2019$stadsdeel %>%
  replace(rent_burden_2019$wijk %in% wijken_zuid, "Zuid")

# Assign 'West' to neighborhoods in West
rent_burden_2019$stadsdeel <- rent_burden_2019$stadsdeel %>%
  replace(rent_burden_2019$wijk %in% wijken_west, "West")

# Read rent prices per district and filter for 2019
huur_long <- read_csv("data/huurprijzen_per_stadsdeel.csv")
huur_long <- huur_long %>% filter(jaar == 2019)

# Remove the 'jaar' column as it's no longer needed after filtering
huur_long$jaar <- NULL

# Display the 'stadsdeel' column (likely for debugging or checking values)
huur_long$stadsdeel

# Merge rent price data with income data based on district
rent_burden_2019 <- rent_burden_2019 %>% 
  full_join(huur_long, by = "stadsdeel")

# Calculate rent burden as percentage of income
rent_burden_2019 <- rent_burden_2019 %>%
  mutate(Rent_burden = huurprijs / gem_inkomen_per_maand * 100)

# Export the resulting dataset to a CSV file
write_csv(rent_burden_2019, "data/rent_burden_2019.csv")
