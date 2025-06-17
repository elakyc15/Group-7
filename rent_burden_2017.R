library(sf)
library(dplyr)
library(ggplot2)

inkomen_per_wijk_2017 <- bind_rows(data_west_2017, data_zuid_2017)
rent_burden_2017 <- inkomen_per_wijk_2017

rent_burden_2017 <- rent_burden_2017 %>% mutate(gem_inkomen_per_maand = gem_inkomen * 1000 / 12)

wijken_zuid <- c('Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
                 'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
                 'Buitenveldert-Oost','Buitenveldert-West','Rijnbuurt','Scheldebuurt',
                 'IJselbuurt', 'Prinses Irenebuurt e.o.')
wijken_west <- c('Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
                 'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
                 'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
                 'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
                 'Westindische Buurt','Vondelbuurt','Centrale Markt', 'Sloterdijk')

rent_burden_2017$stadsdeel <- 0
rent_burden_2017$stadsdeel <- rent_burden_2017$stadsdeel %>%
  replace(rent_burden_2017$wijk %in% wijken_zuid, "Zuid")
rent_burden_2017$stadsdeel <- rent_burden_2017$stadsdeel %>%
  replace(rent_burden_2017$wijk %in% wijken_west, "West")

huur_long <- read_csv("data/huurprijzen_per_stadsdeel.csv")
huur_long <- huur_long %>% filter(jaar == 2017)
huur_long$jaar <- NULL
huur_long$stadsdeel
rent_burden_2017 <- rent_burden_2017 %>% full_join(huur_long, by = "stadsdeel")

rent_burden_2017 <- rent_burden_2017 %>% mutate(Rent_burden = huurprijs / gem_inkomen_per_maand * 100)