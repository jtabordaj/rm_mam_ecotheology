source("./code/01_data_adequation.R")

## 1. Principal Component Analysis (PCA) to obtain the Environmental Index

pcaData <- dataEnvironmental %>% select(envir_econ_priority, envir_efforts_pointless, envir_other_importances, 
    envir_network_effect, envir_threats_exaggerated, envir_protection_money
    )
sapply(pcaData, function(x) length(unique(x)))

pcaModel <- principal(
  pcaData, 
  nfactors = 1, 
  cor = "poly", # Polychoric correlation stimates "hidden" continuous values in categorical variables.
  scores = TRUE
)

# Model Output. [0,1], where 0 approaches high environmental attitude, and 1 approaches less environmental attitude.
# Loading signs. All must be above zero
print(pcaModel$loadings, cutoff = 0.3)
# Accounted variance. Ideally between 0.40 and 0.55
print(pcaModel$Vaccounted) 

# For convenience we put this back again in dataEnvironmental, but is in code section because is as model product.
dataEnvironmental$env_index <- pcaModel$scores[,1]
dataEnvironmental$env_index_scaled <- rescale(
  dataEnvironmental$env_index, 
  to = c(0, 1) 
)

summary(dataEnvironmental$env_index_scaled)





## 2. Regression Analysis

# Ensure NAs in exposure are treated as 0 (Safety check)
dataEnvironmental <- dataEnvironmental %>% 
  mutate(
    franc_exposure_1500 = replace_na(franc_exposure_1500, 0),
    dom_exposure_1500 = replace_na(dom_exposure_1500, 0)
  )

# --- Model 1: Simple Bivariate (Franciscans Only) ---
# Hypothesis: Higher exposure -> Lower Index (More Pro-Environment)
model_1 <- lm(env_index_scaled ~ franc_exposure_1500, data = dataEnvironmental)

message("\n==========================================")
message(" MODEL 1: Franciscans Only")
message("==========================================")
print(summary(model_1))


# --- Model 2: Franciscans vs Dominicans ---
# Do Franciscans differ from Dominicans?
model_2 <- lm(env_index_scaled ~ franc_exposure_1500 + dom_exposure_1500, data = dataEnvironmental)

message("\n==========================================")
message(" MODEL 2: Franciscans vs Dominicans")
message("==========================================")
print(summary(model_2))


# --- Model 3: Country Fixed Effects ---
# Adding Country Fixed Effects (factor(c_abrv))
model_3 <- lm(env_index_scaled ~ franc_exposure_1500 + dom_exposure_1500 + factor(c_abrv), data = dataEnvironmental)

message("\n==========================================")
message(" MODEL 3: Fixed Effects (Hybrid Map)")
message("==========================================")
# Print only the main coefficients to avoid scrolling through 30 country dummies
print(summary(model_3)$coefficients[1:3, ])


## 3. Visualization of Results
library(broom)
library(dplyr)
library(ggplot2)

# Combine results into one table for plotting
models_list <- list(
  "1. Simple" = model_1,
  "2. No FE" = model_2,
  "3. With FE" = model_3
)

plot_data <- bind_rows(lapply(names(models_list), function(model_name) {
  tidy(models_list[[model_name]], conf.int = TRUE) %>%
    filter(term %in% c("franc_exposure_1500", "dom_exposure_1500")) %>%
    mutate(Model = model_name)
}))

# Create the Coefficient Plot
gg_coef <- ggplot(plot_data, aes(x = term, y = estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Impact of Monastic Orders on Environmental Attitudes",
    subtitle = "Negative Coefficient = More Pro-Environmental",
    y = "Effect Size (Std. Devs of Index)",
    x = "Monastic Order"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Display Plot
print(gg_coef)

# Save Plot (Optional)
ggsave("./figures/regression_results.png", plot = gg_coef, width = 8, height = 5)
