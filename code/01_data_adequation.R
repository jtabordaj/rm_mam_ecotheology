source('./code/00_dependencies.R')

## 1. To clean Europe's map
# We decide to remove islands and overseas territories such as Iceland, French peripherals, Azores, Madeira, Canary Islands.
# NUTS 1 specificity
overseasTerritories <- c("^FRY", "^PT2", "^PT3", "^ES7", "^IS") # Umbrella term for anything not close to mainland Europe + Britain
filterStatement <- paste(overseasTerritories, collapse = "|")
mapEurope <- mapEurope %>% mutate(NUTS_ID = as.character(NUTS_ID))
mapEurope <- mapEurope %>% filter(!str_detect(NUTS_ID, filterStatement))

# We also zoom-in the map in Europe, latitudinal/longitudinal coordinates are handcrafted based on the project data
mapEurope <- st_transform(mapEurope, standardCRS)
mapEurope <- st_crop(mapEurope, xmin = -10, xmax = 45, ymin = 0, ymax = 69) 
mapEurope <- st_make_valid(mapEurope)

# NUTS Selection
mapEurope_N2 <- mapEurope %>% filter(LEVL_CODE %in% c(2))
mapEurope_N1 <- mapEurope %>% filter(LEVL_CODE %in% c(1))

# NUTS Hybrid Map. Experimental feature.
germany_N1 <- mapEurope_N1 %>% filter(str_detect(NUTS_ID, "^DE"))
europe_N2_no_DE <- mapEurope_N2 %>% filter(!str_detect(NUTS_ID, "^DE"))

mapEurope <- bind_rows(europe_N2_no_DE, germany_N1)
# plot(st_geometry(mapEurope))

## 2. To enrich NUTS data with Monastery location
map_monasteries(dataFranciscan)
map_monasteries(dataDominican)

## 3. To enrich NUTS data with environmental attitudes
dataEnvironmental <- dataEnvironmental %>% select(
    studyno, doi, studynoc, id_cocas, caseno, year, country, c_abrv, cntry_y, 
    v129, v199, v200, v201, v202, v203, v204, # Environmental Variables
    v275b_N1, v275b_N2, # NUTS Variables
    v225,           # Gender
    age,            # Age (constructed)
    v276_r,         # Town Size
    v243_edulvlb,   # Education
    v261_ppp,       # Income PPP
    v246_ISEI,      # Socio-Economic Status
    v174_LR,        # Political Scale
    v52             # Religion
) 

dataEnvironmental <- dataEnvironmental %>% 
    mutate(
        v275b_N2 = ifelse(v275b_N2 == "-4", v275b_N1, v275b_N2)
    )
#
names(dataEnvironmental)[names(dataEnvironmental) == "v275b_N2"] <- "NUTS_ID"

dataEnvironmental <- dataEnvironmental %>% 
    mutate( 
      # --- Environmental Index Vars ---
      # Mutate Idea: Values further away from 1 indicate less pro-environmental attitudes
        envir_orgs_confidence =  ifelse(v129 %in% c(1, 2, 3, 4), v129, NA), # v129: How much confidence you have in environmental orgs.
        envir_econ_priority = ifelse(v204 %in% c(1, 2), v204, NA), # v204: Growth vs Protection priorities.
        envir_protection_money = ifelse(v199 > 0, v199, NA), # v199: Give income to environmental causes.
        envir_efforts_pointless = ifelse(v200 %in% c(1, 2, 3 , 4 , 5), 6 - v200, NA), # v200: Too difficult for someone like me to do much about the environment. REVERSED DIRECTION
        envir_other_importances = ifelse(v201 %in% c(1, 2, 3 , 4, 5), 6 - v201, NA), # v201: There are more important things to do in life than protect the environment
        envir_network_effect = ifelse(v202 %in% c(1, 2, 3, 4, 5), 6 - v202, NA), # v202: No point in doing what I can for the environment unless others do the same
        envir_threats_exaggerated = ifelse(v203 %in% c(1, 2, 3, 4, 5), 6 - v203, NA), # v203: Many of the claims about environmental threats are exaggerated.
      # --- Control Variables ---
        gender_female = ifelse(v225 == 2, 1, 0),
        age_clean = ifelse(age > 0, age, NA),
        town_size = ifelse(v276_r > 0, v276_r, NA),
        education = ifelse(v243_edulvlb >= 0, v243_edulvlb, NA),
        income_ppp = ifelse(v261_ppp >= 0, v261_ppp, NA),
        isei_status = ifelse(v246_ISEI > 0, v246_ISEI, NA),
        political_right = ifelse(v174_LR >= 1, v174_LR, NA),
        is_catholic = ifelse(v52 == 1, 1, 0),
        is_protestant = ifelse(v52 == 2, 1, 0)
    )
## Note: na.omit() removes rows where ANY variable is NA.

dataEnvironmental <- left_join(
    dataEnvironmental, 
    mapEurope %>% distinct(NUTS_ID, .keep_all = TRUE), 
    by = "NUTS_ID"
)

dataEnvironmental <- dataEnvironmental %>% select(
    studyno, caseno, id_cocas, country, c_abrv, cntry_y, NUTS_ID,
    envir_econ_priority, envir_efforts_pointless, envir_other_importances, 
    envir_network_effect, envir_threats_exaggerated, envir_protection_money,
    gender_female, age_clean, town_size, education, income_ppp, isei_status, 
    political_right, is_catholic, is_protestant,
    LEVL_CODE, NAME_LATN, NUTS_NAME, geometry
    )
#

## 4. Working with HYDE data (3.3 Version, Baseline)
dataHYDE <- dataHYDE[[21:28]] # population_21 = 1000 CE, scales century-wise until population_28 = 1700
names(dataHYDE) <- c("pop_1000", "pop_1100", "pop_1200", "pop_1300", "pop_1400", "pop_1500", "pop_1600", "pop_1700")
dataHYDE <- crop(dataHYDE, mapEurope)
dataPopulation <- terra::extract(dataHYDE, mapEurope, fun = sum, na.rm = TRUE, ID = TRUE)
dataPopulation <- cbind(mapEurope, dataPopulation)

# For medieval Europe I would say extract() does the job well without weights = TRUE or  exact = TRUE (see ?terra::extract)

## 5. Enriching HYDE data
# Pixel value = distance to nearest point (in meters). Note: Distance is geodetic (meters).
# First prepare monastery dates before joining them
dataDominican_date <- dataDominican %>% select(name, founded, lat, lon, start_century)
dataDominican_date <- dataDominican_date %>% st_as_sf(coords = c("lon", "lat"), crs = standardCRS)

dataFranciscan_date <- dataFranciscan_date %>% select(monastery_name, lat, lon, start, end)
dataFranciscan_date$lat <- as.numeric(dataFranciscan_date$lat)
dataFranciscan_date$lon <- as.numeric(dataFranciscan_date$lon)
dataFranciscan_date <- dataFranciscan_date %>% st_as_sf(coords = c("lon", "lat"), crs = standardCRS)
dataFranciscan_date <- dataFranciscan_date %>% mutate(monastery_name = str_to_title(monastery_name))

# For Population
check_create_hyde_data("./cache/dataFranciscan_popExposure.rds", dataFranciscan, dataPopulation, dataFranciscan_date)
check_create_hyde_data("./cache/dataDominican_popExposure.rds", dataDominican, dataPopulation, dataDominican_date)


## 6. Merge Exposure Data into Main Dataset

# A. Process Franciscan Data
franc_clean <- readRDS("./cache/dataFranciscan_popExposure.rds") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(NUTS_ID, exposure_1500) %>% 
  rename(franc_exposure_1500 = exposure_1500) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)

# B. Process Dominican Data
dom_clean <- readRDS("./cache/dataDominican_popExposure.rds") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(NUTS_ID, exposure_1500) %>% 
  rename(dom_exposure_1500 = exposure_1500) %>% 
  distinct(NUTS_ID, .keep_all = TRUE)

# C. Join with Environmental Data
# We use left_join so we keep all survey respondents.
dataEnvironmental <- dataEnvironmental %>%
  left_join(franc_clean, by = "NUTS_ID") %>%
  left_join(dom_clean, by = "NUTS_ID")

# D. Handle Zero Exposure
# If a region (NUTS_ID) does not appear in the monastery list, the join creates NA.
# This implies 0 exposure (no monastery nearby).
exposure_cols <- c("franc_exposure_1500", "dom_exposure_1500")

dataEnvironmental <- dataEnvironmental %>%
  mutate(across(all_of(exposure_cols), ~replace_na(., 0)))

# E. Final Sanity Check
# We check 'id_cocas' which is the unique global ID for respondents.
if(any(duplicated(dataEnvironmental$id_cocas))) {
  warning("Duplicated respondents detected! Check the join logic.")
} else {
  message("Data Adequation Complete. 'dataEnvironmental' is clean and ready.")
}


## 7. Save Final Data

# Option A: Save as CSV (Best for checking in Excel)
write_csv(dataEnvironmental, "./data/final_merged_data.csv")

# Option B: Save as RDS (Best for loading back into R later)
saveRDS(dataEnvironmental, "./data/final_merged_data.rds")
