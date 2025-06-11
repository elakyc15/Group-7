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

#merge data 2017 en 2019
library(dplyr)
library(stringr)

# Function: filter on Zuid/West buurten, clean, group & summarise
summarise_buurten <- function(df, year) {
  df %>%
    # rename the key cols to a standard set
    rename(
      Type_regio       = Regioaanduiding.Soort.regio..omschrijving.,
      Wijken_en_buurten = Wijken.en.buurten,
      Ink40_low        = starts_with("Inkomen.Inkomen.van.huishoudens.40"),
      Ink20_high       = starts_with("Inkomen.Inkomen.van.huishoudens.20"),
      Share_low_inc    = starts_with("Inkomen.Inkomen.van.huishoudens.Huishoudens.met.een.laag")
    ) %>%
    mutate(
      Type_regio = str_trim(Type_regio),
      # keep only wijken
      is_wijk    = str_detect(Type_regio, regex("Wijk", ignore_case = TRUE)),
      # parse numbers (comma → dot)
      Ink40_low     = as.numeric(str_replace(Ink40_low, ",", ".")),
      Ink20_high    = as.numeric(str_replace(Ink20_high, ",", ".")),
      Share_low_inc = as.numeric(str_replace(Share_low_inc, ",", "."))
    ) %>%
    filter(
      is_wijk,
      Wijken_en_buurten %in% c(
        # Zuid
        'Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
        'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
        # West
        'Houthavens','Spaarndammer- en Zeeheldenbuurt','Staatsliedenbuurt',
        'Frederik Hendrikbuurt','Da Costabuurt','Kinkerbuurt','Van Lennepbuurt',
        'Helmersbuurt','Overtoomse Sluis','Chassébuurt','Landlust','Erasmuspark',
        'De Kolenkit','Geuzenbuurt','Van Galenbuurt','Hoofdweg e.o.','Westindische Buurt'
      )
    ) %>%
    # assign stadsdeel based on wijk name
    mutate(
      Stadsdeel = case_when(
        Wijken_en_buurten %in% c(
          'Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
          'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt'
        ) ~ "Zuid",
        TRUE ~ "West"
      )
    ) %>%
    group_by(Stadsdeel) %>%
    summarise(
      Gem40_low    = mean(Ink40_low,     na.rm = TRUE),
      Gem20_high   = mean(Ink20_high,    na.rm = TRUE),
      Gem_share_lo = mean(Share_low_inc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Jaar = year)
}

# Apply to both years and bind
Kerncijfers_all <- bind_rows(
  summarise_buurten(Kerncijfers_wijken_en_buurten_2017_10062025_135511, 2017),
  summarise_buurten(Kerncijfers_wijken_en_buurten_2019_05062025_112613, 2019)
)

# View the result
print(Kerncijfers_all)


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

#correcte huurprijzen
# 1. Laad de dataset
huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)

# 2. Verwijder overbodige rijen (zoals NA-rijen en voetnoten)
huur <- huur[huur$V2 %in% c("E West", "K Zuid"), ]

# 3. Hernoem kolommen voor duidelijkheid (je kunt dit aanpassen indien nodig)
colnames(huur) <- c("Index", "Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p", 
                    "2013_c", "2015_c", "2017_c", "2019_c")

# 4. Selecteer alleen particuliere huurprijzen (kolommen eindigend op "_p")
huur_particulier <- huur[, c("Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p")]


huur_particulier <- huur_particulier %>%
  mutate(Stadsdeel = recode(Stadsdeel, "E West" = "West", "K Zuid" = "Zuid"))

# 5. Print resultaat
print(huur_particulier)



#merge huur en income

library(dplyr)
library(stringr)
library(tidyr)

# 1) Function to filter Zuid/West buurten, clean, group & summarise
summarise_buurten <- function(df, year) {
  df %>%
    rename(
      Type_regio        = Regioaanduiding.Soort.regio..omschrijving.,
      Wijken_en_buurten = Wijken.en.buurten,
      Ink40_low         = starts_with("Inkomen.Inkomen.van.huishoudens.40"),
      Ink20_high        = starts_with("Inkomen.Inkomen.van.huishoudens.20"),
      Share_low_inc     = starts_with("Inkomen.Inkomen.van.huishoudens.Huishoudens.met.een.laag")
    ) %>%
    mutate(
      Type_regio    = str_trim(Type_regio),
      is_wijk       = str_detect(Type_regio, regex("Wijk", ignore_case = TRUE)),
      Ink40_low     = as.numeric(str_replace(Ink40_low, ",", ".")),
      Ink20_high    = as.numeric(str_replace(Ink20_high, ",", ".")),
      Share_low_inc = as.numeric(str_replace(Share_low_inc, ",", "."))
    ) %>%
    filter(
      is_wijk,
      Wijken_en_buurten %in% c(
        # Zuid
        'Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
        'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
        # West
        'Houthavens','Spaarndammer- en Zeeheldenbuurt','Staatsliedenbuurt',
        'Frederik Hendrikbuurt','Da Costabuurt','Kinkerbuurt','Van Lennepbuurt',
        'Helmersbuurt','Overtoomse Sluis','Chassébuurt','Landlust','Erasmuspark',
        'De Kolenkit','Geuzenbuurt','Van Galenbuurt','Hoofdweg e.o.','Westindische Buurt'
      )
    ) %>%
    mutate(
      Stadsdeel = case_when(
        Wijken_en_buurten %in% c(
          'Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
          'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt'
        ) ~ "Zuid",
        TRUE ~ "West"
      )
    ) %>%
    group_by(Stadsdeel) %>%
    summarise(
      Gem40_low    = mean(Ink40_low,     na.rm = TRUE),
      Gem20_high   = mean(Ink20_high,    na.rm = TRUE),
      Gem_share_lo = mean(Share_low_inc, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    mutate(Jaar = year)
}

# 2) Apply for 2017 & 2019
incomes_17_19 <- bind_rows(
  summarise_buurten(Kerncijfers_wijken_en_buurten_2017_10062025_135511, 2017),
  summarise_buurten(Kerncijfers_wijken_en_buurten_2019_05062025_112613, 2019)
)

# 3) Summarise 2015 (only 20% hoogste inkomen available)
incomes_2015 <- Kerncijfers_wijken_en_buurten_2015_10062025_180625 %>%
  rename(
    Type_regio        = Regioaanduiding.Soort.regio..omschrijving.,
    Wijken_en_buurten = Wijken.en.buurten,
    Gem20_high        = starts_with("Inkomen.Inkomen.van.huishoudens.20")
  ) %>%
  mutate(
    Type_regio = str_trim(Type_regio),
    is_wijk    = str_detect(Type_regio, regex("Wijk", ignore_case = TRUE)),
    Gem20_high = as.numeric(str_replace(Gem20_high, ",", "."))
  ) %>%
  filter(
    is_wijk,
    Wijken_en_buurten %in% c(
      # Zuid & West same list as above
      'Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
      'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt',
      'Houthavens','Spaarndammer- en Zeeheldenbuurt','Staatsliedenbuurt',
      'Frederik Hendrikbuurt','Da Costabuurt','Kinkerbuurt','Van Lennepbuurt',
      'Helmersbuurt','Overtoomse Sluis','Chassébuurt','Landlust','Erasmuspark',
      'De Kolenkit','Geuzenbuurt','Van Galenbuurt','Hoofdweg e.o.','Westindische Buurt'
    )
  ) %>%
  mutate(
    Stadsdeel = if_else(
      Wijken_en_buurten %in% c(
        'Zuidas','Oude Pijp','Nieuwe Pijp','Zuid Pijp','Hoofddorppleinbuurt',
        'Schinkelbuurt','Willemspark','Museumkwartier','Stadionbuurt','Apollobuurt'
      ),
      "Zuid", "West"
    )
  ) %>%
  group_by(Stadsdeel) %>%
  summarise(
    Gem20_high   = mean(Gem20_high, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    Gem40_low    = NA_real_,
    Gem_share_lo = NA_real_,
    Jaar         = 2015
  )

# 4) Combine all income summaries
incomes_all <- bind_rows(incomes_2015, incomes_17_19)

# 5) Load & clean huurprijzen.csv
huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)
colnames(huur) <- c(
  "Index","Stadsdeel",
  "2013_p","2015_p","2017_p","2019_p",
  "2013_c","2015_c","2017_c","2019_c"
)
huur_particulier <- huur %>%
  filter(Stadsdeel %in% c("E West","K Zuid")) %>%
  mutate(
    Stadsdeel = recode(Stadsdeel, "E West" = "West", "K Zuid" = "Zuid")
  ) %>%
  select(Stadsdeel, ends_with("_p"))

# 6) Pivot rents long after coercing to numeric
huur_long <- huur_particulier %>%
  mutate(across(matches("\\d{4}_p$"), as.numeric)) %>%
  pivot_longer(
    cols           = -Stadsdeel,
    names_to       = "Jaar",
    names_pattern  = "(\\d{4})_p",
    values_to      = "Huurprijs"
  ) %>%
  mutate(Jaar = as.integer(Jaar))

# 7) Merge incomes + rents
combined <- incomes_all %>%
  left_join(huur_long, by = c("Stadsdeel", "Jaar"))

# Inspect final result
print(combined)





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
