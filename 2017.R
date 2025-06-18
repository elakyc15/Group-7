# Load the dplyr package for data manipulation
library(dplyr)

# Load the 2017 neighborhood statistics dataset
data <- read.csv("data/Kerncijfers_wijken_en_buurten_2017.csv")

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

# Filter the full dataset to include only Zuid neighborhoods
data_zuid_2017 <- data %>%
  filter(Wijken.en.buurten %in% wijken_zuid)

# Select relevant columns (neighborhood name and average income) and rename them for Zuid
data_zuid_2017 <- data_zuid_2017 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )

# Filter the full dataset to include only West neighborhoods
data_west_2017 <- data %>%
  filter(Wijken.en.buurten %in% wijken_west)

# Select relevant columns (neighborhood name and average income) and rename them for West
data_west_2017 <- data_west_2017 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )

# Convert income column to numeric for Zuid (in case it's read as factor or string)
data_zuid_2017$gem_inkomen <- as.numeric(as.character(data_zuid_2017$gem_inkomen))

# Convert income column to numeric for West
data_west_2017$gem_inkomen <- as.numeric(as.character(data_west_2017$gem_inkomen))

# Calculate average income for Zuid and add corresponding district label
avg_zuid_2017 <- data_zuid_2017 %>%
  summarise(
    stadsdeel = "Zuid",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

# Calculate average income for West and add corresponding district label
avg_west_2017 <- data_west_2017 %>%
  summarise(
    stadsdeel = "West",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

# Combine both district summaries into a single dataset
inkomen_2017 <- bind_rows(avg_zuid_2017, avg_west_2017)

# Add the year column to indicate this data is from 2017
inkomen_2017$jaar <- 2017

# Export data to CSV files for further analysis or reporting
write_csv(avg_west_2017, "data/avg_west_2017.csv")
write_csv(avg_zuid_2017, "data/avg_zuid_2017.csv")
write_csv(data_west_2017, "data/data_west_2017.csv")
write_csv(data_zuid_2017, "data/data_zuid_2017.csv")
write_csv(inkomen_2017, "data/inkomen_2017.csv")
