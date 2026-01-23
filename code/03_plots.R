if(!exists("mapEurope") || !exists("dataEnvironmental")) {
  message("Please run '02_model.R' first!")
  source('./code/02_model.R')
}

########################################################
# 1. Monasteries Map (Franciscan & Dominican)
########################################################

dataFranciscan_sf <- st_as_sf(dataFranciscan, coords = c("lon", "lat"), crs = st_crs(mapEurope))
dataDominican_sf <- st_as_sf(dataDominican, coords = c("lon", "lat"), crs = st_crs(mapEurope))

monasteries_sf <- bind_rows(
  dataFranciscan_sf %>% mutate(order = "Franciscan"),
  dataDominican_sf %>% mutate(order = "Dominican")
)

p_monasteries <- ggplot() +
  geom_sf(data = mapEurope, fill = "grey96", color = "grey75", linewidth = 0.1) +
  geom_sf(data = monasteries_sf, aes(color = order), size = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("Franciscan" = "#008080", "Dominican" = "#E65100"), name = "Order") +
  labs(title = "Franciscan and Dominican Houses", subtitle = "Locations of mendicant orders") +
  theme_minimal() +
  theme(legend.position = "right", panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())

########################################################
# 2. HYDE Population Grid (1200-1500)
########################################################

hyde_subset <- dataHYDE[[c("pop_1200", "pop_1300", "pop_1400", "pop_1500")]]
hyde_df <- as.data.frame(hyde_subset, xy = TRUE) %>%
  pivot_longer(cols = starts_with("pop_"), names_to = "year_raw", values_to = "population") %>%
  mutate(year = gsub("pop_", "", year_raw)) %>%
  filter(population > 0)

p_hyde_grid <- ggplot() +
  geom_raster(data = hyde_df, aes(x = x, y = y, fill = population)) +
  geom_sf(data = mapEurope, fill = NA, color = "white", linewidth = 0.05, alpha = 0.3) +
  scale_fill_viridis_c(option = "magma", direction = 1, na.value = "transparent", name = "Pop (Log Scale)", trans = "pseudo_log", breaks = c(0, 100, 1000, 10000, 100000)) +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Population Density (HYDE 3.3)", subtitle = "Grid-level estimates (Log Scale)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())

########################################################
# 3. HYDE Population NUTS2
########################################################

nuts_pop_df <- dataPopulation %>%
  as.data.frame() %>% 
  select(-matches("geom")) %>% 
  select(NUTS_ID, matches("pop_1[2-5]00")) %>%
  pivot_longer(cols = -NUTS_ID, names_to = "year_raw", values_to = "population") %>%
  mutate(year = gsub("pop_", "", year_raw))

nuts_pop_sf <- left_join(mapEurope, nuts_pop_df, by = "NUTS_ID")

p_hyde_nuts1 <- ggplot(nuts_pop_sf) +
  geom_sf(aes(fill = population), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = 1, na.value = "grey90", name = "Total Pop", trans = "sqrt") +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Regional Population (NUTS 2)", subtitle = "Aggregated HYDE 3.3 data") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())

########################################################
# 4a. Franciscan Exposure (1200-1500)
########################################################

exposure_file <- "./cache/dataFranciscan_popExposure.rds"

if(file.exists(exposure_file)) {
  franciscan_exp <- readRDS(exposure_file)
  franciscan_exp$NUTS_ID <- as.character(franciscan_exp$NUTS_ID)

  # 1. Clean and Pivot the Data
  franciscan_long_df <- franciscan_exp %>%
    as.data.frame() %>% 
    select(-matches("geom")) %>% 
    # Select columns starting with "exposure_" followed by the years we want
    select(NUTS_ID, matches("exposure_1[2-5]00")) %>% 
    pivot_longer(cols = -NUTS_ID, names_to = "year_raw", values_to = "exposure") %>%
    mutate(year = gsub("exposure_", "", year_raw))
  
  # 2. Join with mapEurope
  franciscan_sf_plot <- left_join(mapEurope, franciscan_long_df, by = "NUTS_ID") %>%
    filter(!is.na(year))
  
  # 3. Plot
  p_franciscan_exposure <- ggplot(franciscan_sf_plot) +
    geom_sf(aes(fill = exposure), color = NA) +
    
    # --- UPDATED SCALE FOR 0-1 DATA ---
    scale_fill_gradient(
      low = "#e5f5f9",   # Lightest Green (0 / Low)
      high = "#00441b",  # Darkest Green (1 / High)
      na.value = "grey95",
      name = "Exposure\nIndex (0-1)"
      # Removed 'trans="pseudo_log"' because your data is already 0-1
    ) +
    # ----------------------------------
    
    facet_wrap(~ year, ncol = 2) +
    labs(
      title = "Franciscan Exposure (1200–1500)", 
      subtitle = "Normalized Index (0 = None, 1 = High Exposure)"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())

} else {
  p_franciscan_exposure <- NULL
}

########################################################
# 4b. Dominican Exposure (1200-1500) - Faceted
########################################################

# We assume a similar cache file exists for Dominicans. 
# If you generated the Franciscan one in 01_data_adequation.R, the Dominican one should be there too.
dom_exposure_file <- "./cache/dataDominican_popExposure.rds"

if(file.exists(dom_exposure_file)) {
  dominican_exp <- readRDS(dom_exposure_file)
  dominican_exp$NUTS_ID <- as.character(dominican_exp$NUTS_ID)

  # 1. Clean and Pivot
  dominican_long_df <- dominican_exp %>%
    as.data.frame() %>% 
    select(-matches("geom")) %>% 
    select(NUTS_ID, matches("exposure_1[2-5]00")) %>% 
    pivot_longer(cols = -NUTS_ID, names_to = "year_raw", values_to = "exposure") %>%
    mutate(year = gsub("exposure_", "", year_raw))
  
  # 2. Join
  dominican_sf_plot <- left_join(mapEurope, dominican_long_df, by = "NUTS_ID") %>%
    filter(!is.na(year))
  
  # 3. Plot (Using Orange Palette for Dominicans)
  p_dominican_exposure <- ggplot(dominican_sf_plot) +
    geom_sf(aes(fill = exposure), color = NA) +
    scale_fill_gradient(
      low = "#fff5eb",   # Lightest Orange
      high = "#d94801",  # Darkest Orange
      na.value = "grey95",
      name = "Exposure\nIndex (0-1)"
    ) +
    facet_wrap(~ year, ncol = 2) +
    labs(
      title = "Dominican Exposure (1200–1500)", 
      subtitle = "Normalized Index (0 = None, 1 = High Exposure)"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())
  
} else {
  message("Warning: './cache/dataDominican_popExposure.rds' not found. Skipping Dominican faceted plot.")
}


########################################################
# 4c. Presentation Plots - High Visibility
########################################################

# --- A. Franciscan Exposure 1500 (Teal Theme) ---
p_franciscan_pres <- ggplot(dataEnvironmental) +
  # Use the map geometry linked to the data
  geom_sf(data = mapEurope %>% right_join(dataEnvironmental, by="NUTS_ID"), 
          aes(fill = franc_exposure_1500), color = NA) +
  geom_sf(data = mapEurope, fill = NA, color = "white", size=0.1) + # Add borders for clarity
  
  scale_fill_gradient(
    low = "#e0f2f1",   # Very Light Teal
    high = "#004d40",  # Deep Teal (Franciscan Color)
    na.value = "grey92",
    name = "Intensity"
  ) +
  labs(title = "Franciscan Exposure") + # Simple title for slides
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "right"
  )


# --- B. Dominican Exposure 1500 (Orange Theme) ---
p_dominican_pres <- ggplot(dataEnvironmental) +
  geom_sf(data = mapEurope %>% right_join(dataEnvironmental, by="NUTS_ID"), 
          aes(fill = dom_exposure_1500), color = NA) +
  geom_sf(data = mapEurope, fill = NA, color = "white", size=0.1) +
  
  scale_fill_gradient(
    low = "#fff3e0",   # Very Light Orange
    high = "#e65100",  # Deep Orange (Dominican Color)
    na.value = "grey92",
    name = "Intensity"
  ) +
  labs(title = "Dominican Exposure") + 
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "right"
  )


########################################################
# 5. Environmental Questions
########################################################

env_vars_df <- dataEnvironmental %>%
  as.data.frame() %>% 
  select(-matches("geom")) %>% 
  # FIX: Select only environmental variables, but EXCLUDE the scaled ones (_sc) created in script 02
  select(NUTS_ID, starts_with("envir_")) %>%
  select(-ends_with("_sc")) %>%
  
  pivot_longer(cols = starts_with("envir_"), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, 
                           labels = c(
                             "Econ Priority (v204)", "Efforts Pointless (v200)", "Network Effect (v202)", 
                             "Other Importances (v201)", "Protection Money (v199)", 
                             "Threats Exaggerated (v203)"
                           )))

env_sf_plot <- left_join(mapEurope, env_vars_df, by = "NUTS_ID") %>%
  filter(!is.na(variable))

p_env_questions <- ggplot(env_sf_plot) +
  geom_sf(aes(fill = value), color = NA) +
  
  # --- GREEN (Low/Pro) to ORANGE (High/Anti) ---
  scale_fill_gradient(
    low = "darkgreen", 
    high = "orange", 
    na.value = "grey90", 
    name = "Score"
  ) +
  
  facet_wrap(~ variable, ncol = 3) +
  labs(title = "Environmental Attitudes", subtitle = "Dark Green = Pro-Env (Low), Orange = Anti-Env (High)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())

########################################################
# 6. Environmental Index (PCA Result)
########################################################

# 1. Create Regional Averages for the Map
# We group the individual data by NUTS region to get one score per region
region_stats <- dataEnvironmental %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID) %>%
  summarise(
    avg_index = mean(env_index_scaled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(avg_index)) # Remove regions with no data

# 2. Join these averages back to the map geometry
env_index_sf <- left_join(mapEurope, region_stats, by = "NUTS_ID")

# 3. Identify Top 3 (Highest/Anti) and Bottom 3 (Lowest/Pro) REGIONS
top_bottom_labels <- env_index_sf %>%
  filter(!is.na(avg_index)) %>%    # Ensure we only look at valid data
  arrange(avg_index) %>%           # Sort Low -> High
  slice(c(1:3, (n()-2):n())) %>%   # Take 3 lowest and 3 highest regions
  mutate(
    label_text = sprintf("%.3f", avg_index),
    coords = st_centroid(geometry)
  ) %>%
  mutate(
    X = st_coordinates(coords)[,1],
    Y = st_coordinates(coords)[,2]
  )

# 4. Plot
p_env_index <- ggplot(env_index_sf) +
  geom_sf(aes(fill = avg_index), color = NA) +
  
  # --- LABELS LAYER ---
  geom_text(
    data = top_bottom_labels,
    aes(x = X, y = Y, label = label_text),
    color = "black",
    fontface = "bold",
    size = 3,
    check_overlap = FALSE 
  ) +
  # --------------------

  scale_fill_gradient(
    low = "darkgreen",   # Low Score = Pro-Env
    high = "orange",     # High Score = Anti-Env
    na.value = "grey90", 
    name = "Index"
  ) +
  labs(
    title = "Environmental Index (PCA)", 
    subtitle = "Regional Averages: Dark Green = Pro-Env, Orange = Anti-Env"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()
)

########################################################
# Save All Plots
########################################################

ggsave("figures/01_monasteries_map.png", p_monasteries, width = 10, height = 8, bg = "white")
ggsave("figures/02_hyde_grid.png", p_hyde_grid, width = 10, height = 10, bg = "white")
ggsave("figures/03_hyde_nuts.png", p_hyde_nuts1, width = 10, height = 10, bg = "white")

if(!is.null(p_franciscan_exposure)) {
  ggsave("figures/04a_franciscan_exposure.png", p_franciscan_exposure, width = 10, height = 10, bg = "white")
}
ggsave("figures/04b_dominican_exposure.png", p_dominican_exposure, width = 10, height = 10, bg = "white")
ggsave("figures/04c_franciscan_presentation.png", p_franciscan_pres, width = 10, height = 8, bg = "white")
ggsave("figures/04d_dominican_presentation.png", p_dominican_pres, width = 10, height = 8, bg = "white")

ggsave("figures/05_env_questions.png", p_env_questions, width = 12, height = 10, bg = "white")
ggsave("figures/06_env_index.png", p_env_index, width = 8, height = 8, bg = "white")

message("All plots generated and saved successfully!")