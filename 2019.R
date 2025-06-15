library(dplyr)


data <- read.csv("data/Kerncijfers_wijken_en_buurten_2019.csv", dec=",")


wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

data_zuid_2019 <- data %>%
  filter(Wijken.en.buurten %in% wijken_zuid)

data_zuid_2019 <- data_zuid_2019 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )


data_west_2019 <- data %>%
  filter(Wijken.en.buurten %in% wijken_west)

data_west_2019 <- data_west_2019 %>%
  select(
    wijk = Wijken.en.buurten,
    gem_inkomen = Inkomen.Inkomen.van.personen.Gemiddeld.inkomen.per.inkomensontvanger...x.1.000.euro.
  )

avg_zuid_2019 <- data_zuid_2019 %>%
  summarise(
    stadsdeel = "Zuid",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

avg_west_2019 <- data_west_2019 %>%
  summarise(
    stadsdeel = "West",
    gem_inkomen = round(mean(gem_inkomen, na.rm = TRUE), 2)
  )

inkomen_2019 <- bind_rows(avg_zuid_2019, avg_west_2019)

inkomen_2019$jaar <- 2019