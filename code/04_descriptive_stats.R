# A. Monastery Counts
n_franciscans <- nrow(dataFranciscan)
n_dominicans <- nrow(dataDominican)

message("--------------------------------------")
message("Total Franciscan Monasteries: ", n_franciscans)
message("Total Dominican Monasteries:  ", n_dominicans)
message("--------------------------------------")

# B. Variable Summary Table (Robust Version)
library(dplyr)
library(tidyr)

# 1. Select and Rename variables
desc_data <- dataEnvironmental %>%
  select(
    `Environmental Index` = env_index_scaled,
    `Franciscan Exposure` = franc_exposure_1500, 
    `Dominican Exposure` = dom_exposure_1500,
    `Female` = gender_female, 
    `Age` = age_clean, 
    `Education Level` = education, 
    `Income (PPP)` = income_ppp, 
    `Political Right` = political_right, 
    `Town Size` = town_size, 
    `Socio-Economic Index` = isei_status, 
    `Catholic` = is_catholic, 
    `Protestant` = is_protestant
  )

# 2. Calculate Stats
# We use .names = "{.col}__{.fn}" to put a double underscore between name and stat
desc_stats <- desc_data %>%
  summarise(across(everything(), list(
    Mean = ~mean(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    N = ~sum(!is.na(.))
  ), .names = "{.col}__{.fn}")) %>%
  
  # 3. Reshape
  pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
  separate(Key, into = c("Variable", "Stat"), sep = "__") %>% # Split on double underscore
  pivot_wider(names_from = "Stat", values_from = "Value")

# 4. Print and Save
print(as.data.frame(desc_stats), digits = 3)

write.csv(desc_stats, "./figures/descriptive_statistics.csv", row.names = FALSE)
message("Success! Clean statistics saved to ./figures/descriptive_statistics.csv")
