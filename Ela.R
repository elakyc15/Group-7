write.csv(Kerncijfers_wijken_en_buurten_2019_05062025_112613, "data/Kerncijfers_wijken_en_buurten_2019.csv")

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
