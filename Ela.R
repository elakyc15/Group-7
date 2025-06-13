#write.csv(Kerncijfers_wijken_en_buurten_2019_05062025_112613, "data/Kerncijfers_wijken_en_buurten_2019.csv")

# 1. Laad de dataset
huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)

# 2. Verwijder overbodige rijen (zoals NA-rijen en voetnoten)
huur <- huur[huur$V2 %in% c("E West", "K Zuid"), ]

# 3. Hernoem kolommen voor duidelijkheid (je kunt dit aanpassen indien nodig)
colnames(huur) <- c("Index", "Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p", 
                    "2013_c", "2015_c", "2017_c", "2019_c")

# 4. Selecteer alleen particuliere huurprijzen (kolommen eindigend op "_p")
huur_particulier <- huur[, c("Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p")]

library(dplyr)

huur_particulier <- huur_particulier %>%
  mutate(Stadsdeel = recode(Stadsdeel, "E West" = "West", "K Zuid" = "Zuid"))

colnames(huur_particulier) <- sub("_p", "", colnames(huur_particulier)) 

# 5. Print resultaat
print(huur_particulier)


library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Laad de dataset
huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)

# 2. Filter op relevante stadsdelen
huur <- huur[huur$V2 %in% c("E West", "K Zuid"), ]

# 3. Hernoem kolommen
colnames(huur) <- c("Index", "Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p", 
                    "2013_c", "2015_c", "2017_c", "2019_c")

# 4. Selecteer particuliere huurprijzen
huur_particulier <- huur[, c("Stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p")]

# 5. Hernoem stadsdelen
huur_particulier <- huur_particulier %>%
  mutate(Stadsdeel = recode(Stadsdeel, "E West" = "West", "K Zuid" = "Zuid"))

# 6. Hernoem kolommen om '_p' te verwijderen
colnames(huur_particulier) <- sub("_p", "", colnames(huur_particulier))

# Zet alle kolommen behalve 'Stadsdeel' om naar numeric
huur_particulier[, -1] <- lapply(huur_particulier[, -1], as.numeric)

# 7. Zet data in lange vorm
huur_long <- pivot_longer(huur_particulier, 
                          cols = -Stadsdeel, 
                          names_to = "Jaar", 
                          values_to = "Huurprijs")

# 8. Zet Jaar om naar numeriek (voor x-as)
huur_long$Jaar <- as.numeric(huur_long$Jaar)

# 9. Maak de line plot
ggplot(huur_long, aes(x = Jaar, y = Huurprijs, color = Stadsdeel)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Ontwikkeling van particuliere huurprijzen",
       x = "Jaar",
       y = "Huurprijs (index)",
       color = "Stadsdeel") +
  theme_minimal()

