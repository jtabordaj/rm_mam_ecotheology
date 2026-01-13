source("./code/01_data_adequation.R")

## 1. Principal Component Analysis (PCA) to obtain the Environmental Index
pcaModel <- principal(
  dataEnvironmental %>% select(envir_econ_priority, envir_protection_money), 
  nfactors = 1, 
  cor = "poly", # Polychoric correlation stimates "hidden" continuous values in categorical variables.
  scores = TRUE
)

# For convenience we put this back again in dataEnvironmental, but is in code section because is as model product.
dataEnvironmental$env_index <- pcaModel$scores[,1]
dataEnvironmental$env_index_scaled <- rescale(
  dataEnvironmental$env_index, 
  to = c(0, 1) 
)

summary(dataEnvironmental$env_index_scaled)
# [0,1] where  0 approaches high environmental attitude, and 1 approaches less environmental attitude.


