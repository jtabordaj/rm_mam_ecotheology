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
# Join additional data on time of Franciscan monasteries
dataFranciscan_date <- dataFranciscan_date %>% mutate(monastery = str_to_title(monastery))
dataFranciscan_date <- dataFranciscan_date %>% select(monastery, lat, lon, start, end)

enrich_monastery_data(dataFranciscan)
enrich_monastery_data(dataDominican)

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
# pixel value = distance to nearest point (in meters). Note: Distance is geodetic (meters).
enrich_hyde_with_monasteries(dataFranciscan, dataHYDE, dataPopulation, mapEurope, 25000)
