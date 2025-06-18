# Load required libraries
library(sf)        
library(dplyr)     
library(ggplot2)  

# Combine income data for West and Zuid neighborhoods into one dataset
inkomen_per_wijk_2015 <- bind_rows(data_west_2015, data_zuid_2015)

# Create a copy to compute rent burden
rent_burden_2015 <- inkomen_per_wijk_2015

# Calculate monthly income by converting yearly income (in thousands of euros) to monthly euros
rent_burden_2015 <- rent_burden_2015 %>% 
  mutate(gem_inkomen_per_maand = gem_inkomen * 1000 / 12)

# Define neighborhoods in each district
wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

# Add a 'stadsdeel' (district) column and assign 'Zuid' or 'West' based on neighborhood
rent_burden_2015$stadsdeel <- 0
rent_burden_2015$stadsdeel <- rent_burden_2015$stadsdeel %>%
  replace(rent_burden_2015$wijk %in% wijken_zuid, "Zuid")
rent_burden_2015$stadsdeel <- rent_burden_2015$stadsdeel %>%
  replace(rent_burden_2015$wijk %in% wijken_west, "West")

# Load rental price data (in long format) and filter for the year 2015
huur_long <- read_csv("data/huurprijzen_per_stadsdeel.csv")
huur_long <- huur_long %>% filter(jaar == 2015)

# Drop the 'jaar' column (since all rows are now 2015)
huur_long$jaar <- NULL

# Join rental price data to income data by district
rent_burden_2015 <- rent_burden_2015 %>% 
  full_join(huur_long, by = "stadsdeel")

# Calculate rent burden as a percentage of monthly income
rent_burden_2015 <- rent_burden_2015 %>% 
  mutate(Rent_burden = huurprijs / gem_inkomen_per_maand * 100)

# Export final rent burden data for 2015 to CSV
write_csv(rent_burden_2015, "data/rent_burden_2015.csv")
