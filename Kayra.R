write.csv(Kerncijfers_wijken_en_buurten_2019_05062025_112613, "data/Kerncijfers_wijken_en_buurten_2019.csv")
write.csv(Kerncijfers_wijken_en_buurten_2017_10062025_135511,"data/Kerncijfers_wijken_en_buurten_2017.csv")
#overbodige rijen verwijderen
huur <- c("index")



#filteren op buurten in Amsterdam-Zuid en west 2019
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(tidyverse)

Kerncijfers2019 <- Kerncijfers_wijken_en_buurten_2019_05062025_112613

#filter
Kerncijfers2019 <- Kerncijfers2019 %>% filter(str_detect(Regioaanduiding.Soort.regio..omschrijving., "Wijk"))
select(Kerncijfers2019,c())

Kerncijfers2019<-rename(Kerncijfers2019, 'Type regio' = 'Regioaanduiding.Soort.regio..omschrijving.')


