# Load the dplyr package for data manipulation
library(dplyr)

# Load the 2019 dataset; use dec="," to correctly interpret European-style decimal commas
data <- read.csv("data/Kerncijfers_wijken_en_buurten_2019.csv", dec=",")

# Define the neighborhoods (wijken) in the Zuid district
wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')

# Define the neighborhoods in the West district
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

# Filter data to only include Zuid neighborhoods
data_zuid_2019 <- data %>%
  filter(Wijken.en.buurten %in% wijken_zuid)

# Select relevant columns (neighborhood name and average income) and rename them
data_zuid_2019 <- data_zuid_2019 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )

# Filter data to only include West neighborhoods
data_west_2019 <- data %>%
  filter(Wijken.en.buurten %in% wijken_west)

# Select relevant columns (neighborhood name and average income) and rename them
data_west_2019 <- data_west_2019 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )

# Calculate average income for Zuid and label with corresponding district name
avg_zuid_2019 <- data_zuid_2019 %>%
  summarise(
    stadsdeel = "Zuid",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

# Calculate average income for West and label with corresponding district name
avg_west_2019 <- data_west_2019 %>%
  summarise(
    stadsdeel = "West",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

# Combine both district summaries into a single dataset
inkomen_2019 <- bind_rows(avg_zuid_2019, avg_west_2019)

# Add a column indicating the year of the data
inkomen_2019$jaar <- 2019

# Export datasets to CSV files for storage or further use
write_csv(avg_west_2019, "data/avg_west_2019.csv")
write_csv(avg_zuid_2019, "data/avg_zuid_2019.csv")
write_csv(data_west_2019, "data/data_west_2019.csv")
write_csv(data_zuid_2019, "data/data_zuid_2019.csv")
write_csv(inkomen_2019, "data/inkomen_2019.csv")
