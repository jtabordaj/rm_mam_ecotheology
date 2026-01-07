source('./dependencies.R')

## 1. To clean Europe's map
# We decide to remove islands and overseas territories such as Iceland, French peripherals, Azores, Madeira, Canary Islands.
# NUTS 1 specificity
overseasTerritories <- c("^FRY", "^PT2", "^PT3", "^ES7", "^IS") # Umbrella term for anything not close to mainland Europe + Britain
filterStatement <- paste(overseasTerritories, collapse = "|")
mapEurope <- mapEurope %>% mutate(NUTS_ID = as.character(NUTS_ID))
mapEurope <- mapEurope %>% filter(!str_detect(NUTS_ID, filterStatement))
mapEurope <- mapEurope %>% filter(LEVL_CODE %in% c(1)) # Switch for NUTS specificity

# We also zoom-in the map in Europe, latitudinal/longitudinal coordinates are handcrafted based on the project data
mapEurope <- st_transform(mapEurope, standardCRS)
mapEurope <- st_crop(mapEurope, xmin = -10, xmax = 45, ymin = 0, ymax = 69) 
mapEurope <- st_make_valid(mapEurope)

# plot(st_geometry(mapEurope))

## 2. To enrich NUTS data with Monastery location
map_monasteries(dataFranciscan)
map_monasteries(dataDominican)

## 3. To enrich NUTS data with environmental attitudes
dataEnvironmental <- dataEnvironmental %>% select(
    studyno, doi, studynoc, id_cocas, caseno, year, country, c_abrv, cntry_y, 
    v13, v129, v199, v200, v201, v202, v203, v204,
    v275b_N1, v275c_N1
)
names(dataEnvironmental)[names(dataEnvironmental) == "v275b_N1"] <- "NUTS_ID"

dataEnvironmental <- left_join(dataEnvironmental, mapEurope, by = "NUTS_ID")

## 4. Working with HYDE data (3.3 Version, Baseline)
dataHYDE <- dataHYDE[[21:28]] # population_21 = 1000 CE, scales century-wise until population_28 = 1700
names(dataHYDE) <- c("pop_1000", "pop_1100", "pop_1200", "pop_1300", "pop_1400", "pop_1500", "pop_1600", "pop_1700")
dataHYDE <- crop(dataHYDE, mapEurope)
dataPopulation <- terra::extract(dataHYDE, mapEurope, fun = sum, na.rm = TRUE, ID = TRUE)
dataPopulation <- cbind(mapEurope, dataPopulation)

# For medieval Europe I would say extract() does the job well without weights = TRUE or  exact = TRUE (see ?terra::extract)

## 5. Enriching HYDE data
# Pixel value = distance to nearest point (in meters). Note: Distance is geodetic (meters).

# For Population
check_create_hyde_data("./output/dataFranciscan_popExposure.rds", dataFranciscan, dataPopulation)
check_create_hyde_data("./output/dataDominican_popExposure.rds", dataDominican, dataPopulation)

## 6. Adding information on foundation/building date to the complete dataset.
# First we modify the objects directly
dataDominican_date <- dataDominican %>% select(name, founded, lat, lon, start_century)
dataDominican_date <- dataDominican_date %>% st_as_sf(coords = c("lon", "lat"), crs = standardCRS)

dataFranciscan_date <- dataFranciscan_date %>% select(monastery_name, lat, lon, start, end)
dataFranciscan_date$lat <- as.numeric(dataFranciscan_date$lat)
dataFranciscan_date$lon <- as.numeric(dataFranciscan_date$lon)
dataFranciscan_date <- dataFranciscan_date %>% st_as_sf(coords = c("lon", "lat"), crs = standardCRS)
dataFranciscan_date <- dataFranciscan_date %>% mutate(monastery_name = str_to_title(monastery_name))

# Now we append this information on dates to the dataset

# For population
dataFranciscan_popExposure <- st_join(dataFranciscan_popExposure, dataFranciscan_date, join = st_intersects)
dataDominican_popExposure <- st_join(dataDominican_popExposure, dataDominican_date, join = st_intersects)

## Write the final dataset

# For population
write_rds(dataDominican_popExposure, "./output/dataDominican_popExposure.rds")
write_rds(dataFranciscan_popExposure, "./output/dataFranciscan_popExposure.rds")
