# Install the 'stringr' package if it's not already installed
# install.packages("stringr")

# Load necessary libraries for data manipulation and string processing
library(dplyr)
library(stringr)
library(readr)

# Read the first two rows after skipping 3 to create multi-line headers
headers <- read.csv("data/Kerncijfers_wijken_en_buurten_2015.csv", skip = 3, nrows = 2, sep = ";", header = FALSE)

# Combine the two header rows into single strings per column
new_headers <- paste(headers[1, ], headers[2, ], sep = "")  # Join headers with no space (can be customized)

# Replace whitespace with dots to standardize column names
new_headers <- str_replace_all(new_headers, "\\s+", ".")

# Read the actual data starting from the 6th row (skip metadata rows)
data <- read.csv("data/Kerncijfers_wijken_en_buurten_2015.csv", skip = 5, sep = ";", header = FALSE)

# Apply the newly created header names to the dataset
colnames(data) <- new_headers

# Define the neighborhoods (wijken) within the Zuid district
wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')

# Define the neighborhoods within the West district
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

# Filter data to include only neighborhoods from Zuid
data_zuid_2015 <- data %>%
  filter(Wijken.en.buurten %in% wijken_zuid)

# Select relevant columns and rename them for Zuid
data_zuid_2015 <- data_zuid_2015 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = "Inkomen|Inkomen.van.personen|Gemiddeld.inkomen.per.inkomensontvanger.x.1.000.euro"
  )

# Filter data to include only neighborhoods from West
data_west_2015 <- data %>%
  filter(Wijken.en.buurten %in% wijken_west)

# Select relevant columns and rename them for West
data_west_2015 <- data_west_2015 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = "Inkomen|Inkomen.van.personen|Gemiddeld.inkomen.per.inkomensontvanger.x.1.000.euro"
  )

# Convert income values from string (with comma) to numeric for Zuid
data_zuid_2015$gem_inkomen <- as.numeric(gsub(",", ".", data_zuid_2015$gem_inkomen))

# Convert income values from string (with comma) to numeric for West
data_west_2015$gem_inkomen <- as.numeric(gsub(",", ".", data_west_2015$gem_inkomen))

# Calculate the average income for Zuid
avg_zuid_2015 <- data_zuid_2015 %>%
  summarise(
    stadsdeel = "Zuid",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

# Calculate the average income for West
avg_west_2015 <- data_west_2015 %>%
  summarise(
    stadsdeel = "West",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

# Combine the average income data for Zuid and West into one dataset
inkomen_2015 <- bind_rows(avg_zuid_2015, avg_west_2015)

# Add the year as a column to the combined income dataset
inkomen_2015$jaar <- 2015

# Export datasets to CSV files
write_csv(inkomen_2015, "data/inkomen_2015.csv")
write_csv(avg_west_2015, "data/avg_west_2015.csv")
write_csv(avg_zuid_2015, "data/avg_zuid_2015.csv")
write_csv(data_west_2015, "data/data_west_2015.csv")
write_csv(data_zuid_2015, "data/data_zuid_2015.csv")
