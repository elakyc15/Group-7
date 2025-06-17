library(dplyr)


#load data
data <- read.csv("data/Kerncijfers_wijken_en_buurten_2017.csv")


wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

data_zuid_2017 <- data %>%
  filter(Wijken.en.buurten %in% wijken_zuid)

data_zuid_2017 <- data_zuid_2017 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )

data_west_2017 <- data %>%
  filter(Wijken.en.buurten %in% wijken_west)

data_west_2017 <- data_west_2017 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )

data_zuid_2017$gem_inkomen <- as.numeric(as.character(data_zuid_2017$gem_inkomen))
data_west_2017$gem_inkomen <- as.numeric(as.character(data_west_2017$gem_inkomen))

avg_zuid_2017 <- data_zuid_2017 %>%
  summarise(
    stadsdeel = "Zuid",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

avg_west_2017 <- data_west_2017 %>%
  summarise(
    stadsdeel = "West",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

inkomen_2017 <- bind_rows(avg_zuid_2017, avg_west_2017)

inkomen_2017$jaar <- 2017

write_csv(avg_west_2017, "data/avg_west_2017.csv")
write_csv(avg_zuid_2017, "data/avg_zuid_2017.csv")
write_csv(data_west_2017, "data/data_west_2017.csv")
write_csv(data_zuid_2017, "data/data_zuid_2017.csv")
write_csv(inkomen_2017, "data/inkomen_2017.csv")
write_csv(inkomen_per_wijk_2017, "data/inkomen_per_wijk_2017.csv")


