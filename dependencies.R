Sys.setlocale('LC_ALL','en_US.UTF-8')
options(timeout = 600)
options(scipen = 999)
packages <- c("haven", "writexl", "sf", "readr", "readxl", "dplyr", "ggplot2", "stringr", "terra", "tidyr")
packages_to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, dependencies = TRUE)
}
invisible(lapply(packages, library, character.only = TRUE))

# Paths
online_path_hyde <- "https://geo.public.data.uu.nl/vault-hyde/HYDE%203.3%5B1710493486%5D/original/hyde33_c7_base_mrt2023/NetCDF/population.nc"
local_path_hyde <- "./data/hyde/hyde_grid.nc"
local_path_nuts <- "./data/NUTS.shp/NUTS_RG_20M_2021_3035.shp"
local_path_environmental <- "./data/environmental/environmental_attitudes.csv"
local_path_dominican <- "./data/houses/mps_dominican_1216_1500.csv"
local_path_franciscan <- "./data/houses/mps_franciscan_1300.csv"

if (!dir.exists('./data/hyde')) {
  dir.create('./data/hyde', recursive = TRUE)
  message("HYDE Folder created")
} else {
  message("HYDE Folder exists")
}

# Read
mapEurope <- st_read(local_path_nuts)
dataEnvironmental <- read_csv(local_path_environmental, show_col_types = FALSE)
dataDominican <- read_csv(local_path_dominican, show_col_types = FALSE)
dataFranciscan <- read_csv(local_path_franciscan, show_col_types = FALSE)

if (file.exists(local_path_hyde)) {
  message("Loading HYDE...")
  dataHYDE <- rast(local_path_hyde)
  message("...SUCCESS")
} else {
  message("Downloading HYDE...")
  tryCatch({
    download.file(url = online_path_hyde, destfile = "./data/hyde/hyde_grid.nc", mode = "wb")
    dataHYDE <- rast(local_path_hyde)
    message("...SUCCESS")
  }, error = function(e) {
    message("...FAIL")
    print(e)
  })
}

standardCRS <- crs(dataHYDE)

# Functions
enrich_monastery_data <- function(dataset){
  target <- dataset
  target <- target %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
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

enrich_hyde_with_monasteries <- function(order, hyde_grid, population_data, european_map, geodeticThreshold){
  # Get exposure directly from the rasterized monastery data
  rasterOrder <- vect(order, geom = c("lon", "lat"), crs = standardCRS)
  distanceOrder <- distance(hyde_grid[[1]], rasterOrder)
  exposureOrder <- distanceOrder <= geodeticThreshold

  # Overlap the HYDE grid with the exposure
  exposureGrid <- mask(hyde_grid, exposureOrder, maskvalues = 0)
  exposedPopulation <- terra::extract(exposureGrid, european_map, fun = sum, na.rm = TRUE, ID = FALSE)
  colnames(exposedPopulation) <- paste0(colnames(exposedPopulation), "_exposed")
  exposedPopulation <- exposedPopulation %>% mutate(across(ends_with("_exposed"), ~ replace_na(., 0)))
  exposedPopulation <- cbind(population_data, exposedPopulation)

  # Loop to get exposure ratios
  pop_cols <- grep("^pop_[0-9]+$", names(exposedPopulation), value = TRUE)
  pop_year <- gsub("pop_", "", pop_cols)
  for (y in pop_year) {
    total_col <- paste0("pop_", y)            
    exposed_col <- paste0("pop_", y, "_exposed")
    new_col <- paste0("exposure_", y)
    if (total_col %in% names(exposedPopulation) && exposed_col %in% names(exposedPopulation)) {
      exposedPopulation[[new_col]] <- ifelse(
          exposedPopulation[[total_col]] == 0, 
          0, 
          exposedPopulation[[exposed_col]] / exposedPopulation[[total_col]]
      )
    }
  }
  assign(paste(deparse(substitute(order)), "_complete", sep = ""), exposedPopulation, envir = .GlobalEnv)
}