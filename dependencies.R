Sys.setlocale('LC_ALL','en_US.UTF-8')
packages <- c("haven", "writexl", "sf", "readr", "readxl", "dplyr", "ggplot2", "stringr")
packages_to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, dependencies = TRUE)
}
invisible(lapply(packages, library, character.only = TRUE))
file_path_nuts <- "./data/NUTS.shp/NUTS_RG_20M_2021_3035.shp"

mapEurope <- st_read(file_path_nuts)
dataEnvironmental <- read_csv("./data/environmental/environmental_attitudes.csv")
dataDominican <- read_csv("./data/houses/mps_dominican_1216_1500.csv")
dataFranciscan <- read_csv("./data/houses/mps_franciscan_1300.csv")

# df <- read_sav("./data/Environmental_attitudes.sav")
# df_numeric <- zap_labels(df)
# write_csv(df_numeric, './data/Environmental_Attitudes.csv')