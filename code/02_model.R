source("./code/01_data_adequation.R")

##############################
## 1. Principal Component Analysis (PCA) to obtain the Environmental Index
##############################

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




##############################
## 2. Regression (Index)
##############################

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


# --- Model 4: Full Specification (Fixed Effects + Individual Controls) ---
model_4 <- lm(
  env_index_scaled ~ 
    franc_exposure_1500 + dom_exposure_1500 + 
    # The 8 Controls:
    gender_female + age_clean + education + income_ppp + political_right + 
    town_size + isei_status + is_catholic + is_protestant + 
    # Country Fixed Effects:
    factor(c_abrv), 
  data = dataEnvironmental
)

message("\n==========================================")
message(" MODEL 4: Full Controls + FE")
message("==========================================")
# We print the first 15 coefficients so you can see the Main Vars AND the key Controls
print(summary(model_4)$coefficients[1:16, ])


## 3. Plot of estimates (Clustered SEs)
library(broom)
library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)

# Helper function to extract clustered estimates for plotting
get_clustered_stats <- function(model, name) {
  # Calculate clustered SEs
  coeftest_res <- coeftest(model, vcov = vcovCL, cluster = ~NUTS_ID)
  
  est <- coeftest_res[, 1]
  se <- coeftest_res[, 2]
  
  data.frame(
    term = names(est),
    estimate = est,
    conf.low = est - 1.96 * se,
    conf.high = est + 1.96 * se,
    Model = name
  )
}

# Combine data
plot_data <- bind_rows(
  get_clustered_stats(model_1, "1. Simple"),
  get_clustered_stats(model_2, "2. No FE"),
  get_clustered_stats(model_3, "3. With FE"),
  get_clustered_stats(model_4, "4. Full Controls")
) %>% 
  filter(term %in% c("franc_exposure_1500", "dom_exposure_1500"))

# Create Plot
gg_coef <- ggplot(plot_data, aes(x = term, y = estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Impact of Monastic Orders (Clustered Standard Errors)",
    subtitle = "95% Confidence Intervals clustered by Region. Crossing 0 = Not Significant.",
    y = "Effect Size (Std. Devs)",
    x = "Monastic Order"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

# Display and Save
print(gg_coef)
ggsave("./figures/07_regressions_plot_4_models.png", plot = gg_coef, width = 8, height = 5)


## 4. Regression Table
if (!require("modelsummary")) install.packages("modelsummary")
if (!require("gt")) install.packages("gt")
if (!require("webshot2")) install.packages("webshot2")
library(modelsummary)
library(gt)

# Define the list of models and their custom titles
models_table <- list(
  "Model 1: Franciscans Only" = model_1,
  "Model 2: Fran vs Dom" = model_2,
  "Model 3: Country FE" = model_3,
  "Model 4: Full Controls + FE" = model_4
)

# Define clean names for variables
coef_map <- c(
  "franc_exposure_1500" = "Franciscan Exposure",
  "dom_exposure_1500" = "Dominican Exposure",
  "gender_female" = "Gender (Female)",
  "age_clean" = "Age",
  "education" = "Education Level",
  "income_ppp" = "Income (PPP)",
  "political_right" = "Political (Right)",
  "town_size" = "Town Size",
  "isei_status" = "Socio-Economic Index",
  "is_catholic" = "Catholic",
  "is_protestant" = "Protestant",
  "(Intercept)" = "Intercept"
)

# Create and Save the Table
msummary(
  models_table,
  coef_map = coef_map,
  vcov = ~NUTS_ID,
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = "AIC|BIC|Log.Lik.|F|RMSE", 
  output = "./figures/08_regressions_table_4_models.png",
  title = "Regression Results (SE Clustered by Region)"
)

message("Success! Table saved to ./figures/08_regressions_table_4_models.png")


##############################
## Individual Questions Regressions
##############################
# Step A: Rescale DVs to 0-1 (0 = Pro-Environment, 1 = Anti-Environment)
library(scales)

vars_to_test <- c("envir_econ_priority", "envir_protection_money", 
                  "envir_efforts_pointless", "envir_other_importances", 
                  "envir_network_effect", "envir_threats_exaggerated")

# Create scaled versions of the variables
for(var in vars_to_test) {
  new_name <- paste0(var, "_sc")
  dataEnvironmental[[new_name]] <- rescale(dataEnvironmental[[var]], to = c(0, 1))
}

# Step B: Run the 6 Regressions (Model 4 Specification)

# 1. Economy vs Environment
m_econ <- lm(envir_econ_priority_sc ~ 
               franc_exposure_1500 + dom_exposure_1500 + 
               gender_female + age_clean + education + income_ppp + political_right + 
               town_size + isei_status + is_catholic + is_protestant + factor(c_abrv), 
             data = dataEnvironmental)

# 2. Willingness to give Income
m_money <- lm(envir_protection_money_sc ~ 
                franc_exposure_1500 + dom_exposure_1500 + 
                gender_female + age_clean + education + income_ppp + political_right + 
                town_size + isei_status + is_catholic + is_protestant + factor(c_abrv), 
              data = dataEnvironmental)

# 3. Efforts are Pointless
m_pointless <- lm(envir_efforts_pointless_sc ~ 
                    franc_exposure_1500 + dom_exposure_1500 + 
                    gender_female + age_clean + education + income_ppp + political_right + 
                    town_size + isei_status + is_catholic + is_protestant + factor(c_abrv), 
                  data = dataEnvironmental)

# 4. Other problems more important
m_other <- lm(envir_other_importances_sc ~ 
                franc_exposure_1500 + dom_exposure_1500 + 
                gender_female + age_clean + education + income_ppp + political_right + 
                town_size + isei_status + is_catholic + is_protestant + factor(c_abrv), 
              data = dataEnvironmental)

# 5. No point unless others do
m_network <- lm(envir_network_effect_sc ~ 
                  franc_exposure_1500 + dom_exposure_1500 + 
                  gender_female + age_clean + education + income_ppp + political_right + 
                  town_size + isei_status + is_catholic + is_protestant + factor(c_abrv), 
                data = dataEnvironmental)

# 6. Threats are exaggerated
m_threats <- lm(envir_threats_exaggerated_sc ~ 
                  franc_exposure_1500 + dom_exposure_1500 + 
                  gender_female + age_clean + education + income_ppp + political_right + 
                  town_size + isei_status + is_catholic + is_protestant + factor(c_abrv), 
                data = dataEnvironmental)


# Step C: Export Results to Table (PNG)
library(modelsummary)
library(gt)

robustness_models <- list(
  "1. Econ Priority" = m_econ,
  "2. Give Money" = m_money,
  "3. Pointless" = m_pointless,
  "4. Other Import." = m_other,
  "5. Network Eff." = m_network,
  "6. Threats Exag." = m_threats
)

# Variables
coef_map_robust <- c(
  "franc_exposure_1500" = "Franciscan Exposure",
  "dom_exposure_1500" = "Dominican Exposure",
  "gender_female" = "Gender (Female)",
  "age_clean" = "Age",
  "education" = "Education",
  "income_ppp" = "Income (PPP)",
  "isei_status" = "Socio-Economic Index",
  "town_size" = "Town Size",
  "political_right" = "Political Orientation (Right)",
  "is_catholic" = "Catholic",
  "is_protestant" = "Protestant"
)

# Create and Save the Table
msummary(
  robustness_models,
  coef_map = coef_map_robust,
  vcov = ~NUTS_ID, 
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = "AIC|BIC|Log.Lik.|F|RMSE",
  output = "./figures/09_regressions_individual_questions.png",
  title = "Individual Environmental Questions (Clustered SEs)"
)

message("Success! Full table saved to ./figures/09_regressions_individual_questions.png")



# Check for Clustered Standard Errors
if (!require("sandwich")) install.packages("sandwich")
if (!require("lmtest")) install.packages("lmtest")

library(sandwich)
library(lmtest)

message("--- Standard Errors (Naive) ---")
print(coeftest(model_4)[1:3, ])

message("\n--- Standard Errors (Clustered by NUTS Region) ---")
print(coeftest(model_4, vcov = vcovCL, cluster = ~NUTS_ID)[1:3, ])


##############################
## ROBUSTNESS CHECKS
##############################
library(modelsummary)
library(sandwich)
library(lmtest)
library(ggplot2)
library(dplyr)
library(broom)

# --- Check 1: Catholics Only ---
# We filter for Catholics and remove religion controls
data_catholic <- dataEnvironmental %>% filter(is_catholic == 1)

model_catholic <- lm(
  env_index_scaled ~ 
    franc_exposure_1500 + dom_exposure_1500 + 
    gender_female + age_clean + education + income_ppp + 
    political_right + town_size + isei_status + 
    factor(c_abrv), 
  data = data_catholic
)

# --- Check 2: Exclude Germany ---
# Germany had special NUTS handling, so we check if results hold without it.
data_no_germany <- dataEnvironmental %>% filter(c_abrv != "DE")

model_no_de <- lm(
  env_index_scaled ~ 
    franc_exposure_1500 + dom_exposure_1500 + 
    gender_female + age_clean + education + income_ppp + 
    political_right + town_size + isei_status + is_catholic + is_protestant +
    factor(c_abrv), 
  data = data_no_germany
)

# --- Export Table: Main vs Checks ---
robustness_list <- list(
  "Main Model (All)" = model_4,
  "Catholics Only" = model_catholic,
  "Exclude Germany" = model_no_de
)

msummary(
  robustness_list,
  coef_map = c("franc_exposure_1500" = "Franciscan Exposure",
               "dom_exposure_1500" = "Dominican Exposure"),
  vcov = ~NUTS_ID,
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = "AIC|BIC|Log.Lik.|F|RMSE",
  output = "./figures/10_robustness_subsamples.png",
  title = "Robustness Checks: Subsamples"
)
message("Subsample table saved to ./figures/10_robustness_subsamples.png")


# --- Check 3: Leave-One-Out Analysis ---
# We run the model 30 times, dropping one country each time.
# If the coefficient changes wildly, that country is driving the result.

countries <- unique(as.character(dataEnvironmental$c_abrv))
results_leave_one_out <- data.frame()

message("Running Leave-One-Out analysis (this may take a minute)...")

for(cntry in countries) {
  # 1. Filter out the specific country
  data_subset <- dataEnvironmental %>% filter(c_abrv != cntry)
  
  # 2. Run the model
  model_temp <- lm(
    env_index_scaled ~ 
      franc_exposure_1500 + dom_exposure_1500 + 
      gender_female + age_clean + education + income_ppp + 
      political_right + town_size + isei_status + is_catholic + is_protestant + 
      factor(c_abrv), 
    data = data_subset
  )
  
  # 3. Get Clustered SEs
  res <- coeftest(model_temp, vcov = vcovCL, cluster = ~NUTS_ID)
  
  # 4. Extract Franciscan & Dominican stats
  # We use tryCatch in case a country is too small and breaks the model
  tryCatch({
    franc_est <- res["franc_exposure_1500", 1]
    franc_se  <- res["franc_exposure_1500", 2]
    dom_est   <- res["dom_exposure_1500", 1]
    dom_se    <- res["dom_exposure_1500", 2]
    
    results_leave_one_out <- rbind(results_leave_one_out, data.frame(
      Country_Dropped = cntry,
      Order = "Franciscan",
      Estimate = franc_est,
      Lower = franc_est - 1.96 * franc_se,
      Upper = franc_est + 1.96 * franc_se
    ))
    
    results_leave_one_out <- rbind(results_leave_one_out, data.frame(
      Country_Dropped = cntry,
      Order = "Dominican",
      Estimate = dom_est,
      Lower = dom_est - 1.96 * dom_se,
      Upper = dom_est + 1.96 * dom_se
    ))
  }, error = function(e) { return(NULL) })
}

# --- Plot the Leave-One-Out Results ---
gg_jack <- ggplot(results_leave_one_out, aes(x = Country_Dropped, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Order, scales = "free_y", ncol = 1) +
  labs(
    title = "Does one country drive the results?",
    subtitle = "Coefficients when excluding each country one-by-one (Clustered 95% CI)",
    y = "Coefficient Estimate",
    x = "Excluded Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(gg_jack)
ggsave("./figures/11_robustness_leave_one_out.png", plot = gg_jack, width = 8, height = 8)

message("leave_one_out plot saved to ./figures/11_robustness_leave_one_out.png")

