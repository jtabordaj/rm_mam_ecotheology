library(haven)
library(writexl)
library(readr)
library(sf)

# df <- read_sav("./data/Environmental_attitudes.sav")
# df_numeric <- zap_labels(df)
# write_csv(df_numeric, './data/Environmental_Attitudes.csv')

file_path <- "./data/NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp"
map_data <- st_read(file_path)
print(map_data)
head(map_data)
names(map_data)
plot(st_geometry(map_data))