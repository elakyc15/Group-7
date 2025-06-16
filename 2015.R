#install.packages("stringr")
library(dplyr)
library(stringr)

headers <- read.csv("data/Kerncijfers_wijken_en_buurten_2015.csv", skip = 3, nrows = 2, sep = ";", header = FALSE)

new_headers <- paste(headers[1, ], headers[2, ], sep = "")  # Customize the separator as needed
new_headers <- str_replace_all(new_headers, "\\s+",".")

data <- read.csv("data/Kerncijfers_wijken_en_buurten_2015.csv", skip = 5, sep = ";", header = FALSE)

colnames(data) <- new_headers

wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

data_zuid_2015 <- data %>%
  filter(Wijken.en.buurten %in% wijken_zuid)

data_zuid_2015 <- data_zuid_2015 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = "Inkomen|Inkomen.van.personen|Gemiddeld.inkomen.per.inkomensontvanger.x.1.000.euro"
  )

data_west_2015 <- data %>%
  filter(Wijken.en.buurten %in% wijken_west)

data_west_2015 <- data_west_2015 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = "Inkomen|Inkomen.van.personen|Gemiddeld.inkomen.per.inkomensontvanger.x.1.000.euro"
  )

data_zuid_2015$gem_inkomen <- as.numeric(gsub(",", ".", data_zuid_2015$gem_inkomen))
data_west_2015$gem_inkomen <- as.numeric(gsub(",", ".", data_west_2015$gem_inkomen))

avg_zuid_2015 <- data_zuid_2015 %>%
  summarise(
    stadsdeel = "Zuid",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

avg_west_2015 <- data_west_2015 %>%
  summarise(
    stadsdeel = "West",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

inkomen_2015 <- bind_rows(avg_zuid_2015, avg_west_2015)

inkomen_2015$jaar <- 2015

write_csv(inkomen_2015, "data/inkomen_2015.csv")
write_csv(avg_west_2015, "avg_west_2015.csv")
