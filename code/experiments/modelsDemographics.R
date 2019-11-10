library(caret)
library(h2o)
library(tidyverse)
library(vip)

h2o.init()   

set.seed(123)  

joinedDataSet <- read.csv("joinedDataSet.csv")
joinedDataSet$topic <- as.factor(joinedDataSet$topic)

# downsampling the data 
balancedData <- downSample(x = joinedDataSet[, -1],
                           y = joinedDataSet$topic)

# split data into training and test 
index <- createDataPartition(balancedData$topic, p = 0.7, 
                               list = FALSE)
trainData <- balancedData[index, ]
testData  <- balancedData[-index, ]

# convert data to h2o objects
trainH2o <- as.h2o(trainData)
testH2o <- as.h2o(testData)

# specifying variables
response <- "topic"

predictors <- trainData %>% 
  select(-document, -gamma, -location, -Class, -topic, 
         -date, -zip, -outlet) %>% 
  colnames()

nFeatures <- length(predictors)


### RANDOM FOREST ###

# OOB model
h2oRF1 <- h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = trainH2o, 
  ntrees = 1000, 
  max_depth = 30, 
  min_rows = 1, 
  sample_rate = 0.8, 
  nfolds = 10,
  fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE,
  seed = 123, 
  stopping_rounds = 50, 
  stopping_metric = "misclassification",
  stopping_tolerance = 0
)

# hyperparameter tuning
hyperGridRF <- list(
  mtries = c(-1, 1, 2, 3),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

searchCriteriaRF <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "misclassification",
  stopping_tolerance = 0.001,   # stop if we don't experience 0.1% improvement
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*10      # or cut grid search off at 10 minutes
)

randomGridRF <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors, 
  y = response, 
  training_frame = trainH2o,
  hyper_params = hyperGridRF,
  ntrees = nFeatures * 10,
  seed = 123,
  stopping_metric = "misclassification",   
  stopping_rounds = 10,           # stop adding trees if we don't experience
  stopping_tolerance = 0.005,     # 0.05 improvement in error over last 10 trees
  search_criteria = searchCriteriaRF
)

gridPerformanceRF <- h2o.getGrid(
  grid_id = "rf_random_grid", 
  sort_by = "mean_per_class_error", 
  decreasing = FALSE
)



### GBM ###
h2oGBM <- h2o.gbm(
  x = predictors, 
  y = response, 
  training_frame = trainH2o, 
  ntrees = 500, 
  learn_rate = 0.01,
  max_depth = 7, 
  min_rows = 5, 
  sample_rate = 0.8, 
  nfolds = 10,
  fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE,
  seed = 123, 
  stopping_rounds = 50, 
  stopping_metric = "misclassification",
  stopping_tolerance = 0
)

# hyperparameter tuning
hyperGridGBM <- list(
  sample_rate = c(0.5, 0.75, 1),              # row subsampling
  col_sample_rate = c(0.5, 0.75, 1),          # col subsampling for each split
  col_sample_rate_per_tree = c(0.5, 0.75, 1)  # col subsampling for each tree
)

searchCriteriaGBM <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "misclassification",
  stopping_tolerance = 0.001,   
  stopping_rounds = 10,         
  max_runtime_secs = 60*60      
)

gridPerformanceGBM <- h2o.getGrid(
  grid_id = "gbm_grid", 
  sort_by = "mean_per_class_error", 
  decreasing = FALSE
)


### STACKING MODELS ###
# model
ensembleTree <- h2o.stackedEnsemble(
  x = predictors, y = response, training_frame = trainH2o, model_id = "example_2",
  base_models = list(h2oRF1, h2oGBM),
  metalearner_algorithm = "drf"
)

# evaluation
getMisclass <- function(model) {
  results <- h2o.performance(model, newdata = testH2o)
  results@metrics$mean_per_class_error
}
list(h2oRF1, h2oGBM) %>%
  purrr::map_dbl(getMisclass)

h2o.performance(ensembleTree, newdata = testH2o)@metrics$mean_per_class_error


vip(h2oRF1)
vip(h2oGBM)


