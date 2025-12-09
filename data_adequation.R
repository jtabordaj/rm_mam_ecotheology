source('./dependencies.R')

## 1. To clean Europe's map
# We remove islands and overseas territories, such as Iceland, French peripherals, Azores, Madeira, Canary Islands.
# NUTS 2 specificity

overseasTerritories <- c("^FRY", "^PT2", "^PT3", "^ES7", "^IS") # Umbrella term for anything not close to mainland Europe + Britain
filterStatement <- paste(overseasTerritories, collapse = "|")
mapEurope <- mapEurope %>% filter(!str_detect(NUTS_ID, filterStatement))
mapEurope <- mapEurope %>% filter(LEVL_CODE %in% c(2))

# We also zoom-in the map in Europe, latitudinal/longitudinal coordinates are handcrafted based on the project data

mapEurope <- st_transform(mapEurope, 4326)
mapEurope <- st_crop(mapEurope, xmin = -10, xmax = 45, ymin = 0, ymax = 69) 
mapEurope <- st_make_valid(mapEurope)
plot(st_geometry(mapEurope))

# Prompt used in Gemini 3.0 for this section: Give me the furthest cardinal points of the European continent, constrained to NUTS 2 regions.
# We then adjusted +-1 or 2 degrees based on existing data

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

## 4. Working with HYDE data (3.3 Version, Baseline)

print(names(dataHYDE))
dataHYDE <- dataHYDE[[21:28]] # population_21 = 1000 CE, scales century-wise until population_28 = 1700
names(dataHYDE) <- c("pop_1000", "pop_1100", "pop_1200", "pop_1300", "pop_1400", "pop_1500", "pop_1600", "pop_1700")
dataHYDE <- crop(dataHYDE, mapEurope)
dataPopulation <- extract(dataHYDE, mapEurope, fun = sum, na.rm = TRUE, ID = TRUE)
dataPopulation <- cbind(mapEurope, dataPopulation)

# For medieval Europe I would say extract() does the job well without weights = TRUE or  exact = TRUE (see ?terra::extract)
# There may be inaccuracies in small regions (see London and Low Countries)
# Prompt used in Gemini 3.0 for this section: Give me a generic code that overlaps NUTS 2 data with an S4 SpatRaster object. We then modified as needed
# In theory, row order should maintain across the section, so row 1 in dataPopulation corresponds to mapEurope row 1. Would love to have a second opinion
