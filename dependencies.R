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

enrich_monastery_data <- function(dataset){
  target <- dataset
  target <- target %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  target <- st_transform(target, 3035)
  join <- st_join(target, mapEurope, join = st_intersects)
  join <- coerceNAPoints(join)
  assign(paste(
    deparse(substitute(dataset)),"_NUTS", sep =""
    ), 
    join,
    envir = .GlobalEnv
  )
}

coerceNAPoints <- function(dataset){
  missingPoints <- dataset %>% filter(is.na(NUTS_ID))
  nearestRegion <- st_nearest_feature(missingPoints, mapEurope)
  completedData <- missingPoints %>% mutate (
    NUTS_ID = mapEurope$NUTS_ID[nearestRegion],
    NUTS_NAME = mapEurope$NUTS_NAME[nearestRegion]
  ) 
  dataset <- dataset %>% filter(!is.na(NUTS_ID)) %>% bind_rows(completedData)
  return(dataset)
}