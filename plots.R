source('./data_adequation.R')

# Preamble: Save Europe Map
png(filename = "figures/nuts2.png", width = 1200, height = 1200, res = 150)
plot(st_geometry(mapEurope))
dev.off()

########################################################
# 1. Map of Franciscan and Dominican houses
########################################################

# Convert coords to sf
dataFranciscan_sf <- st_as_sf(
  dataFranciscan,
  coords = c("lon", "lat"),
  crs = standardCRS
)

dataDominican_sf <- st_as_sf(
  dataDominican,
  coords = c("lon", "lat"),
  crs = standardCRS
)

# Combine for plotting
monasteries_sf <- bind_rows(
  dataFranciscan_sf %>% mutate(order = "Franciscan"),
  dataDominican_sf %>% mutate(order = "Dominican")
)

p_monasteries <- ggplot() +
  geom_sf(data = mapEurope, fill = "grey96", color = "grey75", linewidth = 0.1) +
  geom_sf(data = monasteries_sf, aes(color = order), size = 0.8, alpha = 0.7) +
  scale_color_manual(
    values = c("Franciscan" = "#008080", "Dominican" = "#E65100"),
    name = "Order"
  ) +
  labs(
    title = "Franciscan and Dominican Houses (1300–1500)",
    subtitle = "Locations of mendicant orders"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

print(p_monasteries)


########################################################
# 2. Exposure to Franciscans (Calculated Variable)
########################################################

p_exposure <- ggplot(dataFranciscan_complete) +
  # Plot the exposure_1500 variable
  geom_sf(aes(fill = exposure_1500), color = NA) +
  
  # "mako" palette gives a beautiful range of Greens/Blues/Teals
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,  # Darker colors = Higher exposure
    na.value = "grey92",
    name = "Franciscan\nExposure"
  ) +
  labs(
    title = "Franciscan Exposure (1500)",
    subtitle = "Calculated based on HYDE population density and distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

print(p_exposure)

########################################################
# 3. Environmental attitudes (Standard Join)
########################################################

# Data Prep: Reverse coding and standardizing IDs to character
dataEnvironmental_clean <- dataEnvironmental %>%
  mutate(
    v200_r = -v200,
    v201_r = -v201,
    v202_r = -v202,
    v203_r = -v203,
    NUTS_ID = as.character(NUTS_ID)
  )

# Calculate NUTS-2 Averages
env_nuts2 <- dataEnvironmental_clean %>%
  group_by(NUTS_ID) %>%
  summarise(
    env_v13  = mean(v13,  na.rm = TRUE),
    env_v129 = mean(v129, na.rm = TRUE),
    env_v199 = mean(v199, na.rm = TRUE),
    env_v200 = mean(v200_r, na.rm = TRUE),
    env_v201 = mean(v201_r, na.rm = TRUE),
    env_v202 = mean(v202_r, na.rm = TRUE),
    env_v203 = mean(v203_r, na.rm = TRUE),
    env_v204 = mean(v204, na.rm = TRUE),
    .groups = "drop"
  )

# Strict Left Join (No fallbacks)
mapEurope_env <- left_join(mapEurope, env_nuts2, by = "NUTS_ID")

# Prepare Long Format & Z-Scores
env_long <- mapEurope_env %>%
  select(NUTS_ID, geometry, starts_with("env_")) %>%
  pivot_longer(
    cols = starts_with("env_"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  # Standardize (Z-score) to fix color scaling issues
  group_by(variable) %>%
  mutate(z_score = as.numeric(scale(value))) %>%
  ungroup() %>%
  mutate(
    variable = factor(
      variable,
      levels = c("env_v13", "env_v129", "env_v199", "env_v200",
                 "env_v201", "env_v202", "env_v203", "env_v204"),
      labels = c(
        "Membership in orgs (Q4E)",
        "Confidence in orgs (Q38O)",
        "Willingness to give income (Q56A)",
        "Env. too difficult (Q56B, rev)",
        "Other things important (Q56C, rev)",
        "No point acting alone (Q56D, rev)",
        "Threats exaggerated (Q56E, rev)",
        "Env. vs. Growth (Q57)"
      )
    )
  )

# Faceted Plot
p_env_faceted <- ggplot(env_long) +
  geom_sf(aes(fill = z_score), color = NA) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    na.value = "grey90", # Missing data will be grey
    name = "Standardized\nAttitude (Z-Score)"
  ) +
  facet_wrap(~ variable, ncol = 4) +
  labs(
    title = "Environmental Attitudes Across Europe",
    subtitle = "Z-Scores (0 = Average). Grey areas indicate missing data or ID mismatches."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

print(p_env_faceted)

########################################################
# 4. HYDE Population Grid (1200-1500)
########################################################

# 1. Select specific years from the raster
hyde_subset <- dataHYDE[[c("pop_1200", "pop_1300", "pop_1400", "pop_1500")]]

# 2. Convert Raster to DataFrame (Pixels)
hyde_df <- as.data.frame(hyde_subset, xy = TRUE) %>%
  pivot_longer(
    cols = starts_with("pop_"),
    names_to = "year_raw",
    values_to = "population"
  ) %>%
  mutate(
    year = gsub("pop_", "", year_raw)
  ) %>%
  filter(!is.na(population)) %>% 
  filter(population > 0)

# 3. Plot Grids (Log Scale)
p_hyde_grid <- ggplot() +
  geom_raster(data = hyde_df, aes(x = x, y = y, fill = population)) +
  geom_sf(data = mapEurope, fill = NA, color = "white", linewidth = 0.05, alpha = 0.2) +
  scale_fill_viridis_c(
    option = "magma",
    direction = 1,
    na.value = "transparent",
    name = "Population\n(Log Scale)",
    trans = "pseudo_log", 
    breaks = c(0, 100, 1000, 10000, 100000)
  ) +
  facet_wrap(~ year, ncol = 2) +
  labs(
    title = "Population Density (HYDE 3.3)",
    subtitle = "Grid-level estimates (85km² resolution)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

print(p_hyde_grid)


########################################################
# 5. HYDE Population NUTS-2 (Aggregated)
########################################################

# 1. Prepare Long Data from the dataPopulation sf object
hyde_nuts_long <- dataPopulation %>%
  select(NUTS_ID, geometry, pop_1200, pop_1300, pop_1400, pop_1500) %>%
  pivot_longer(
    cols = starts_with("pop_"),
    names_to = "year_raw",
    values_to = "population"
  ) %>%
  mutate(
    year = gsub("pop_", "", year_raw)
  )

# 2. Plot NUTS-2 Map (Sqrt Scale usually works best for regions)
p_hyde_nuts <- ggplot(hyde_nuts_long) +
  geom_sf(aes(fill = population), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    direction = 1,
    na.value = "grey90",
    name = "Total\nPopulation",
    trans = "sqrt",  # Sqrt helps smooth the gap between big regions and small ones
    breaks = c(50000, 500000, 2000000),
    labels = scales::comma
  ) +
  facet_wrap(~ year, ncol = 2) +
  labs(
    title = "Regional Population Estimates (NUTS-2)",
    subtitle = "Aggregated HYDE 3.3 data per region"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

print(p_hyde_nuts)

########################################################
# 6. Save All Plots
########################################################
ggsave("figures/monasteries_map.png", p_monasteries, width = 10, height = 8, dpi = 300, bg="white")
ggsave("figures/franciscan_exposure_1500.png", p_exposure, width = 10, height = 8, dpi = 300, bg="white")
ggsave("figures/environmental_attitudes.png", p_env_faceted, width = 14, height = 8, dpi = 300, bg="white")
ggsave("figures/hyde_population_grid.png", p_hyde_grid, width = 10, height = 10, dpi = 300, bg="white")
ggsave("figures/hyde_population_nuts2.png", p_hyde_nuts, width = 10, height = 10, dpi = 300, bg="white")