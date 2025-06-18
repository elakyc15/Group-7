# Load necessary libraries
library(sf)        
library(dplyr)     
library(ggplot2)   

# Combine income data from West and Zuid neighborhoods for the year 2017
inkomen_per_wijk_2017 <- bind_rows(data_west_2017, data_zuid_2017)

# Copy data to a new object to calculate rent burden
rent_burden_2017 <- inkomen_per_wijk_2017

# Compute monthly income from annual income (convert from thousands of euros to euros per month)
rent_burden_2017 <- rent_burden_2017 %>% 
  mutate(gem_inkomen_per_maand = gem_inkomen * 1000 / 12)

# Define neighborhood names for Zuid and West districts
wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

# Initialize 'stadsdeel' column and assign district names based on neighborhood
rent_burden_2017$stadsdeel <- 0
rent_burden_2017$stadsdeel <- rent_burden_2017$stadsdeel %>%
  replace(rent_burden_2017$wijk %in% wijken_zuid, "Zuid")
rent_burden_2017$stadsdeel <- rent_burden_2017$stadsdeel %>%
  replace(rent_burden_2017$wijk %in% wijken_west, "West")

# Load rent price data and filter for the year 2017
huur_long <- read_csv("data/huurprijzen_per_stadsdeel.csv")
huur_long <- huur_long %>% filter(jaar == 2017)

# Remove the 'jaar' column since it's no longer needed after filtering
huur_long$jaar <- NULL

# Preview stadsdeel column (likely for debugging or inspection)
huur_long$stadsdeel

# Join rent prices to income data by district
rent_burden_2017 <- rent_burden_2017 %>% 
  full_join(huur_long, by = "stadsdeel")

# Calculate rent burden as percentage of monthly income
rent_burden_2017 <- rent_burden_2017 %>% 
  mutate(Rent_burden = huurprijs / gem_inkomen_per_maand * 100)

# Save the resulting rent burden data for 2017 to a CSV file
write_csv(rent_burden_2017, "data/rent_burden_2017.csv")
