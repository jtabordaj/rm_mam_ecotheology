Sys.setlocale('LC_ALL','en_US.UTF-8')
options(timeout = 600)
options(scipen = 999)
packages <- c("haven", "writexl", "sf", "readr", "readxl", "dplyr", "ggplot2", "stringr", "terra", "tidyr", "psych", "scales")
packages_to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, dependencies = TRUE)
}
invisible(lapply(packages, library, character.only = TRUE))

# Paths
online_path_hyde <- "https://geo.public.data.uu.nl/vault-hyde/HYDE%203.3%5B1710493486%5D/original/hyde33_c7_base_mrt2023/NetCDF/population.nc"
local_path_hyde <- "./data/hyde/hyde_grid.nc"
local_path_nuts <- "./data/NUTS.shp/NUTS_RG_20M_2021_3035.shp"
local_path_environmental <- "./data/environmental/environmental_attitudes.dta"
local_path_dominican <- "./data/houses/mps_dominican_1216_1500.csv"
local_path_franciscan_date <- "./data/houses/novel_franciscan_1300_nuts.xlsx"
local_path_franciscan <- "./data/houses/mps_franciscan_1300.csv"

if(!dir.exists("./figures")){
  dir.create("./figures")
  message("Figures Folder created")
} else {
    message("Figures Folder exists")
  }

if (!dir.exists('./data/hyde')) {
  dir.create('./data/hyde', recursive = TRUE)
  message("HYDE Folder created")
} else {
  message("HYDE Folder exists")
}

if (!dir.exists("./cache")) {
  dir.create("./cache")
  message("Cache Folder created")
} else {
  message("Cache Folder exists.")
}

# Read
mapEurope <- st_read(local_path_nuts)
dataEnvironmental <- read_dta(local_path_environmental) %>% zap_labels()
dataDominican <- read_csv(local_path_dominican, show_col_types = FALSE)
dataFranciscan <- read_csv(local_path_franciscan, show_col_types = FALSE)
dataFranciscan_date <- read_xlsx(local_path_franciscan_date, sheet = "Franciscans")

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
map_monasteries <- function(dataset){
  # Receives houses dataset and returns a dataset with house data on NUTS regions to the global environment.
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
  # Receives a dataset with NUTS_ID and coerces points not in the map to the nearest NUTS region.
  missingPoints <- dataset %>% filter(is.na(NUTS_ID))
  nearestRegion <- st_nearest_feature(missingPoints, mapEurope)
  completedData <- missingPoints %>% mutate (
    NUTS_ID = mapEurope$NUTS_ID[nearestRegion],
    NUTS_NAME = mapEurope$NUTS_NAME[nearestRegion]
  ) 
  dataset <- dataset %>% filter(!is.na(NUTS_ID)) %>% bind_rows(completedData)
  return(dataset)
}

# Functions

enrich_hyde_with_monasteries <- function(order, hyde_grid, population_data, european_map, geodeticThreshold){
  # Receives an order dataset, a HYDE grid, its population, a map and a threshold of exposure
tryCatch({
    # 1. Clean inputs to prevent "non-numeric argument" errors
    # We ensure coordinates are strictly numeric before creating the vector
    order$lat <- as.numeric(order$lat)
    order$lon <- as.numeric(order$lon)
    
    # 2. Prepare Terra objects
    rasterOrder <- terra::vect(order, geom = c("lon", "lat"), crs = standardCRS)
    # Ensure hyde_grid is a SpatRaster (safe wrapper)
    r_grid <- if (inherits(hyde_grid, "SpatRaster")) hyde_grid else terra::rast(hyde_grid)
    # 3. Calculate Distance
    # Explicitly use terra::distance to avoid namespace conflicts with raster::distance
    distanceOrder <- terra::distance(r_grid[[1]], rasterOrder)
    exposureOrder <- distanceOrder <= geodeticThreshold

    exposureGrid <- terra::mask(r_grid, exposureOrder, maskvalues = 0)
    exposedPopulation <- terra::extract(exposureGrid, european_map, fun = sum, na.rm = TRUE, ID = FALSE)
    colnames(exposedPopulation) <- paste0(colnames(exposedPopulation), "_exposed")
    exposedPopulation <- exposedPopulation %>% mutate(across(ends_with("_exposed"), ~ replace_na(., 0)))
    exposedPopulation <- cbind(population_data, exposedPopulation)
    
    # 4. Loop to get exposure ratios
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
    return(exposedPopulation)
  }, error = function(e) {
    message("...FAIL in enrich_hyde_with_monasteries")
    print(e)
    return(NULL)
  })
}

check_create_hyde_data <- function(filePath, order, variable, foundationDates){
  # Checks if we have a complete dataset for a variable saved to the local folder, loads it if true, creates it if false.
  tryCatch({
    objName <- gsub(".rds", "", basename(filePath))
    if(file.exists(filePath)){
      message(paste("Object ", objName, " already exists, reading from disk ", filePath, sep = ""))
      diskRead <- read_rds(filePath)
      assign(objName, diskRead, envir = .GlobalEnv)
      message("...SUCCESS")
    } else {
      message(paste("Object ", objName, " does not exist, creating...", sep = ""))
      enriched_data <- enrich_hyde_with_monasteries(order, dataHYDE, variable, mapEurope, 25000)
      if (is.null(enriched_data)) stop("Enrichment step returned NULL.")
      enriched_data_with_date <- st_join(enriched_data, foundationDates, join = st_intersects)
      assign(objName, enriched_data_with_date, envir = .GlobalEnv)
      write_rds(enriched_data_with_date, filePath)
      message("...SUCCESS")
    } 
  }, error = function(e) {
    message("...FAIL in check_create_hyde_data")
    print(e)
  })
}