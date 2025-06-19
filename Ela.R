# Load required package
library(dplyr)

# Load rental price data, skipping the first row which likely contains metadata
huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)

# Keep only rows for West (E West) and Zuid (K Zuid)
huur <- huur[huur$V2 %in% c("E West", "K Zuid"), ]

# Rename columns for clarity
colnames(huur) <- c("Index", "stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p", 
                    "2013_c", "2015_c", "2017_c", "2019_c")

# Select only private sector rent columns and district names
huur_particulier <- huur[, c("stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p")]

# Recode district names to simplified labels
huur_particulier <- huur_particulier %>%
  mutate(stadsdeel = recode(stadsdeel, "E West" = "West", "K Zuid" = "Zuid"))

# Remove '_p' suffix from column names
colnames(huur_particulier) <- sub("_p", "", colnames(huur_particulier)) 

# Convert year columns to numeric
huur_particulier[, -1] <- lapply(huur_particulier[, -1], as.numeric)

# Reshape data to long format: one row per district-year with rental price
huur_long <- pivot_longer(huur_particulier, 
                          cols = -stadsdeel, 
                          names_to = "jaar", 
                          values_to = "huurprijs")

#library(dplyr)
#library(tidyr)
#library(ggplot2)

# 1. Laad de dataset
#huur <- read.csv("data/huurprijzen.csv", skip = 1, header = FALSE)

# 2. Filter op relevante stadsdelen
#huur <- huur[huur$V2 %in% c("E West", "K Zuid"), ]

# 3. Hernoem kolommen
#colnames(huur) <- c("Index", "stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p", 
                    #"2013_c", "2015_c", "2017_c", "2019_c")

# 4. Selecteer particuliere huurprijzen
#huur_particulier <- huur[, c("stadsdeel", "2013_p", "2015_p", "2017_p", "2019_p")]

# 5. Hernoem stadsdelen
#huur_particulier <- huur_particulier %>%
  #mutate(stadsdeel = recode(stadsdeel, "E West" = "West", "K Zuid" = "Zuid"))

# 6. Hernoem kolommen om '_p' te verwijderen
#colnames(huur_particulier) <- sub("_p", "", colnames(huur_particulier))



# 8. Zet jaar om naar numeriek (voor x-as)
huur_long$jaar <- as.numeric(huur_long$jaar)

# 9. Maak de line plot
#ggplot(huur_long, aes(x = jaar, y = huurprijs, color = stadsdeel)) +
  #geom_line(linewidth = 1.2) +
  #geom_point(size = 2) +
  #labs(title = "Ontwikkeling van particuliere huurprijzen",
       #x = "Jaar",
       #y = "Huurprijs (index)",
       #color = "Stadsdeel") +
  #theme_minimal()

write_csv(huur_long, "data/huurprijzen_per_stadsdeel.csv")