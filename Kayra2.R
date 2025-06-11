write.csv(Kerncijfers_wijken_en_buurten_2019_05062025_112613, "data/Kerncijfers_wijken_en_buurten_2019.csv")

write.csv(Kerncijfers_wijken_en_buurten_2017_10062025_135511,"data/Kerncijfers_wijken_en_buurten_2017.csv")
#overbodige rijen verwijderen
huur <- c("index")

Kerncijfers2019_wijken <- Kerncijfers2019 %>% 
  filter(str_detect('Type regio', "Wijk")) %>% 
  select('Wijken.en.buurten'
         ,'Type regio',
         'Gem. gestandaardiseerd inkomen',
         '40% laagste inkomen','20% hoogste inkomen',
         'Inkomen va huishoudens met laag inkomen')

install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(tidyverse)

#filteren op buurten in Amsterdam-Zuid en west 2019

Kerncijfers2019 <- Kerncijfers_wijken_en_buurten_2019_05062025_112613

Kerncijfers2019<-rename(Kerncijfers2019, 'Type regio' = 'Regioaanduiding.Soort.regio..omschrijving.')
Kerncijfers2019<-rename(Kerncijfers2019,'Gem. gestandaardiseerd inkomen'= 'Inkomen.Inkomen.van.huishoudens.Gem..gestandaardiseerd.inkomen.van.huish..x.1.000.euro.')
Kerncijfers2019<-rename(Kerncijfers2019,'40% laagste inkomen'= 'Inkomen.Inkomen.van.huishoudens.40..huishoudens.met.laagste.inkomen....')
Kerncijfers2019<-rename(Kerncijfers2019,'20% hoogste inkomen'= 'Inkomen.Inkomen.van.huishoudens.20..huishoudens.met.hoogste.inkomen....')
Kerncijfers2019<-rename(Kerncijfers2019,'Inkomen van huishoudens met laag inkomen'= 'Inkomen.Inkomen.van.huishoudens.Huishoudens.met.een.laag.inkomen....')

Kerncijfers2019_wijken <- Kerncijfers2019 %>%
  mutate(`Type regio` = str_trim(`Type regio`)) %>%
  filter(str_detect(`Type regio`, regex("Wijk", ignore_case = TRUE))) %>%
  filter(`Wijken.en.buurten` %in% c(
    # Zuid
    'Zuidas', 'Oude Pijp', 'Nieuwe Pijp', 'Zuid Pijp', 'Hoofddorppleinbuurt',
    'Schinkelbuurt', 'Willemspark', 'Museumkwartier', 'Stadionbuurt', 'Apollobuurt',
    # West
    'Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
    'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
    'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
    'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
    'Westindische Buurt'
  )) %>%
  select(
    `Wijken.en.buurten`,
    `Type regio`,
    `40% laagste inkomen`,
    `20% hoogste inkomen`,
    `Inkomen va huishoudens met laag inkomen`
  ) %>%
  mutate(
    Stadsdeel = case_when(
      `Wijken.en.buurten` %in% c(
        'Zuidas', 'Oude Pijp', 'Nieuwe Pijp', 'Zuid Pijp', 'Hoofddorppleinbuurt',
        'Schinkelbuurt', 'Willemspark', 'Museumkwartier', 'Stadionbuurt', 'Apollobuurt'
      ) ~ "Zuid",
      `Wijken.en.buurten` %in% c(
        'Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
        'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
        'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
        'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
        'Westindische Buurt'
      ) ~ "West"
    ),
    `40% laagste inkomen` = as.numeric(str_replace(`40% laagste inkomen`, ",", ".")),
    `20% hoogste inkomen` = as.numeric(str_replace(`20% hoogste inkomen`, ",", ".")),
    `Inkomen va huishoudens met laag inkomen` = as.numeric(str_replace(`Inkomen va huishoudens met laag inkomen`, ",", "."))
  )  %>%
  group_by(Stadsdeel) %>%
  summarise(
    `Gemiddelde 40% laagste inkomen` = mean(`40% laagste inkomen`, na.rm = TRUE),
    `Gemiddelde 20% hoogste inkomen` = mean(`20% hoogste inkomen`, na.rm = TRUE),
    `Gemiddeld aandeel huishoudens met laag inkomen` = mean(`Inkomen va huishoudens met laag inkomen`, na.rm = TRUE)
  )


#filteren op Amsterdam-zuid en west in 2017
Kerncijfers2017 <- Kerncijfers_wijken_en_buurten_2017_10062025_135511

Kerncijfers2017<-rename(Kerncijfers2017, 'Type regio' = 'Regioaanduiding.Soort.regio..omschrijving.')
Kerncijfers2017<-rename(Kerncijfers2017,'40% laagste inkomen'= 'Inkomen.Inkomen.van.huishoudens.40..huishoudens.met.laagste.inkomen....')
Kerncijfers2017<-rename(Kerncijfers2017,'20% hoogste inkomen'= 'Inkomen.Inkomen.van.huishoudens.20..huishoudens.met.hoogste.inkomen....')
Kerncijfers2017<-rename(Kerncijfers2017,'Inkomen van huishoudens met laag inkomen'= 'Inkomen.Inkomen.van.huishoudens.Huishoudens.met.een.laag.inkomen....')

Kerncijfers2017_wijken <- Kerncijfers2017 %>%
  mutate(`Type regio` = str_trim(`Type regio`)) %>%
  filter(str_detect(`Type regio`, regex("Wijk", ignore_case = TRUE))) %>%
  filter(`Wijken.en.buurten` %in% c(
    # Zuid
    'Zuidas', 'Oude Pijp', 'Nieuwe Pijp', 'Zuid Pijp', 'Hoofddorppleinbuurt',
    'Schinkelbuurt', 'Willemspark', 'Museumkwartier', 'Stadionbuurt', 'Apollobuurt',
    # West
    'Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
    'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
    'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
    'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
    'Westindische Buurt'
  )) %>%
  select(
    `Wijken.en.buurten`,
    `Type regio`,
    `40% laagste inkomen`,
    `20% hoogste inkomen`,
    `Inkomen van huishoudens met laag inkomen`
  )%>%
  mutate(
    Stadsdeel = case_when(
      `Wijken.en.buurten` %in% c(
        'Zuidas', 'Oude Pijp', 'Nieuwe Pijp', 'Zuid Pijp', 'Hoofddorppleinbuurt',
        'Schinkelbuurt', 'Willemspark', 'Museumkwartier', 'Stadionbuurt', 'Apollobuurt'
      ) ~ "Zuid",
      `Wijken.en.buurten` %in% c(
        'Houthavens', 'Spaarndammer- en Zeeheldenbuurt', 'Staatsliedenbuurt',
        'Frederik Hendrikbuurt', 'Da Costabuurt', 'Kinkerbuurt', 'Van Lennepbuurt',
        'Helmersbuurt', 'Overtoomse Sluis', 'Chassébuurt', 'Landlust', 'Erasmuspark',
        'De Kolenkit', 'Geuzenbuurt', 'Van Galenbuurt', 'Hoofdweg e.o.',
        'Westindische Buurt'
      ) ~ "West"
    ),
    `40% laagste inkomen` = as.numeric(str_replace(`40% laagste inkomen`, ",", ".")),
    `20% hoogste inkomen` = as.numeric(str_replace(`20% hoogste inkomen`, ",", ".")),
    `Inkomen van huishoudens met laag inkomen` = as.numeric(str_replace(`Inkomen van huishoudens met laag inkomen`, ",", "."))
  ) %>%
  group_by(Stadsdeel) %>%
  summarise(
    `Gemiddelde 40% laagste inkomen` = mean(`40% laagste inkomen`, na.rm = TRUE),
    `Gemiddelde 20% hoogste inkomen` = mean(`20% hoogste inkomen`, na.rm = TRUE),
    `Gemiddeld aandeel huishoudens met laag inkomen` = mean(`Inkomen van huishoudens met laag inkomen`, na.rm = TRUE)
  )

  
#filteren op Amsterdam-zuid en west 2015
Kerncijfers2015 <- Kerncijfers_wijken_en_buurten_2015_10062025_180625

Kerncijfers2015<-rename(Kerncijfers2015, 'Type regio' = 'Regioaanduiding.Soort.regio..omschrijving.')
Kerncijfers2015<-rename(Kerncijfers2015,'20% hoogste inkomen'= 'Inkomen.Inkomen.van.huishoudens.20..huishoudens.met.hoogste.inkomen....')


Kerncijfers2015_wijken <- Kerncijfers2015 %>%
  mutate(`Type regio` = str_trim(`Type regio`)) %>%  # Trim spaces
  filter(str_detect(`Type regio`, regex("Wijk", ignore_case = TRUE))) %>%
  select(
    `Wijken.en.buurten`,
    `Type regio`,
    `20% hoogste inkomen`,
  )

# 1. Laad de dataset
huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)

# 2. Verwijder overbodige rijen (zoals NA-rijen en voetnoten)
huur <- huur[huur$V2 %in% c("E West", "K Zuid"), ]

# 3. Hernoem kolommen voor duidelijkheid (je kunt dit aanpassen indien nodig)
colnames(huur) <- c("Index", "Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p", 
                    "2013_c", "2015_c", "2017_c", "2019_c")

# 4. Selecteer alleen particuliere huurprijzen (kolommen eindigend op "_p")
huur_particulier <- huur[, c("Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p")]

# 5. Print resultaat
print(huur_particulier)

library(tidyverse)

# Stap 2: Filter alleen op E West en K Zuid (particuliere huurprijzen)
huur <- huur %>%
  filter(V2 %in% c("E West", "K Zuid")) %>%
  select(V2, V5, V6)  # V5 = 2017 (particulier), V6 = 2019 (particulier)

# Stap 3: Hernoem kolommen
colnames(huur) <- c("stadsdeel", "2017", "2019")

# Stap 4: Pivot naar lange vorm
huur_lang <- huur %>%
  pivot_longer(cols = c("2017", "2019"),
               names_to="jaar",
               values_to = "huurprijs")

# Stap 5: Controleer resultaat
print(huur_lang)



huur_particulier$jaar <- 0
>>>>>>> 0d1c11599d7f9c993eacc0849e064567ff2bcb3e
