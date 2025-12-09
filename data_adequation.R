source('./dependencies.R')

## 1. To clean Europe's map
# We remove islands and overseas territories, such as Iceland, French peripherals, Azores, Madeira, Canary Islands.
# We also decide on NUTS 2 specificity

overseasTerritories <- c("^FRY", "^PT2", "^PT3", "^ES7", "^IS") # Umbrella term for anything not close to mainland Europe + Britain
filterStatement <- paste(overseasTerritories, collapse = "|")
mapEurope <- mapEurope %>% filter(!str_detect(NUTS_ID, filterStatement))
mapEurope <- mapEurope %>% filter(LEVL_CODE %in% c(2))

# We also zoom-in the map in Europe, latitudinal/longitudinal coordinates are handcrafted based on the project data

mapEurope <- st_transform(mapEurope, 4326)
mapEurope <- st_crop(mapEurope, xmin = -10, xmax = 45, ymin = 0, ymax = 69)
mapEurope <- st_make_valid(mapEurope)
plot(st_geometry(mapEurope))

## 2. To enrich NUTS data with Monastery location

enrich_monastery_data(dataFranciscan)
enrich_monastery_data(dataDominican)

## 3. To enrich NUTS data with environmental attitudes
# We select the columns relevant to our project and rename v275b_N2, which corresponds to NUTS 2 ID, to NUTS_ID as in mapEurope

dataEnvironmental <- dataEnvironmental %>% select(
    studyno, doi, studynoc, id_cocas, caseno, year, country, c_abrv, cntry_y, 
    v13, v129, v199, v200, v201, v202, v203, v204,
    v275b_N2, v275c_N2
)
names(dataEnvironmental)[names(dataEnvironmental) == "v275b_N2"] <- "NUTS_ID"

dataEnvironmental <- left_join(dataEnvironmental, mapEurope, by = "NUTS_ID")
