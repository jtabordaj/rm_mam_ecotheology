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





