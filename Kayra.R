install.packages("readxl")
library(readxl)
df= read_xlsx("~/Downloads/2021_jaarboek_huurprijs_sochuur_corporatie_particulier_cd55c8d4ec.xlsx")

write.csv(df, "data/huurprijzen.csv")