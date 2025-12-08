source('./dependencies.R')

## 1. To clean Europe's map
# We remove islands and overseas territories, such as Iceland, French peripherals, Azores, Madeira, Canary Islands.
# We also decide on NUTS 2 specificity

overseasTerritories <- c("^FRY", "^PT2", "^PT3", "^ES7", "^IS")
filterStatement <- paste(overseasTerritories, collapse = "|")
mapEurope <- mapEurope %>% filter(!str_detect(NUTS_ID, filterStatement))
mapEurope <- mapEurope %>% filter(LEVL_CODE %in% c(2))

# We also zoom-in the map in Europe, latitudinal/longitudinal coordinates are handcrafted based on the project data

mapEurope <- st_transform(mapEurope, 4326)
mapEurope <- st_crop(mapEurope, xmin = -10, xmax = 45, ymin = 0, ymax = 69)
mapEurope <- st_transform(mapEurope, 3035)
st_make_valid(mapEurope)
plot(st_geometry(mapEurope))

## 2. To enrich NUTS data with Monastery location
# First, Franciscan and Dominican latitudes and longitudes become a geometrical object through the following lines

dataFranciscan <- dataFranciscan %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
dataDominican <- dataDominican %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

dataFranciscan <- st_transform(dataFranciscan, 3035)
dataDominican <- st_transform(dataDominican, 3035)

test <- st_join(dataFranciscan, mapEurope, join = st_intersects)

#

mapEurope <- st_make_valid(mapEurope)

# 2. Filter for just London (or a point that is failing) from your points
london_point <- dataFranciscan %>% 
  filter(grepl("London", monastery_name)) # Replace with your actual column name

# 3. Visual Diagnostic
# We plot the map borders in grey and your point in red
ggplot() +
  # Plot the map (limited to UK area to zoom in)
  geom_sf(data = mapEurope, fill = "white", color = "black") +
  # Plot the point
  geom_sf(data = london_point, color = "red", size = 3) +
  # Zoom in on Northern Europe to see clearly
  coord_sf(xlim = c(2500000, 4500000), ylim = c(2500000, 4500000), crs = 3035) +
  theme_minimal()