library(haven)
library(writexl)
library(readr)
df <- read_sav("./data/Environmental_attitudes.sav")
df_numeric <- zap_labels(df)

write_csv(df_numeric, './data/Environmental_Attitudes.csv')
