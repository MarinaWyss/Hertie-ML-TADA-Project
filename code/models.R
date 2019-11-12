library(caret)
library(h2o)
library(tidyverse)


h2o.init()   

set.seed(123)  

data <- read.csv("preppedDataSet.csv")
ideology <- read.csv("ideology.csv")
data <- merge(data, ideology)

data$topic <- as.factor(data$topic)
data$X <- NULL

# upsampling the data 
balancedData <- upSample(x = data,
                         y = data$topic)

# split data into training and test 
index <- createDataPartition(data$topic, p = 0.7, 
                             list = FALSE)
trainData <- data[index, ]
testData  <- data[-index, ]

# convert data to h2o objects
trainH2o <- as.h2o(trainData)
testH2o <- as.h2o(testData)

# specifying variables
response <- "topic"
predictors <- "ideology"

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

h2oRF1 
# OOB RMSE is 0.7343532, around 73 percent
# OOM MSE is 0.539

# hyperparameter tuning to search a larger grid space for best possible model
hyperGridRF <- list(
  mtries = c(-1, 1, 2, 3),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

#set a random grid search to determine time constraints for finding best model
searchCriteriaRF <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "misclassification",
  stopping_tolerance = 0.001,   # stop if we don't experience 0.1% improvement
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*10      # or cut grid search off at 10 minutes
)

#perform grid search
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

# collect the results and sort by our model performance metric 
# of choice
gridPerformanceRF <- h2o.getGrid(
  grid_id = "rf_random_grid", 
  sort_by = "mean_per_class_error", 
  decreasing = FALSE
)

gridPerformanceRF
#the grid assessed 140 models before stopping due to time
#best model acheived mean per class error of: 0.7016590998715492, ~70 percent
#best model has 10 max depth, 3 minimum rows, -1 mtries, 0.632 sample rate

#adopting our RF to best hyperparameters
# OOB model
h2oRF2 <- h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = trainH2o, 
  ntrees = 1000, 
  max_depth = 10, 
  min_rows = 3, 
  sample_rate = 0.632, 
  nfolds = 10,
  fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE,
  seed = 123, 
  stopping_rounds = 50, 
  stopping_metric = "misclassification",
  stopping_tolerance = 0
)

h2oRF2
#mean per class error is 0.7033
#RMSE is 0.7343413
# nearly the sampe RMSE and MSE as first random forest, but still

# Now let’s evaluate the best hyperparameter model performance on a test set
h2oRF3 <- h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = testH2o, 
  ntrees = 1000, 
  max_depth = 10, 
  min_rows = 3, 
  sample_rate = 0.632, 
  nfolds = 10,
  fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE,
  seed = 123, 
  stopping_rounds = 50, 
  stopping_metric = "misclassification",
  stopping_tolerance = 0
)

h2oRF3
#RMSE on test set is: 0.7406932 - 0.01 improvement from training results
#MSE on test set is: 0.5486265


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

h2oGBM

# model stopped after xx trees
h2oGBM@parameters$ntrees
## [1] 460

# cross validated RMSE
h2o.rmse(h2oGBM, xval = TRUE)
## [1] 0.7377526 - slightly higher than random forest RMSE on training set


# hyperparameter tuning
hyperGridGBM <- list(
  sample_rate = c(0.5, 0.75, 1),              # row subsampling
  col_sample_rate = c(0.5, 0.75, 1),          # col subsampling for each split
  col_sample_rate_per_tree = c(0.5, 0.75, 1)  # col subsampling for each tree
)

#random grid search strategy
searchCriteriaGBM <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "misclassification",
  stopping_tolerance = 0.001,   
  stopping_rounds = 10,         
  max_runtime_secs = 60*60    #one hour  
)

#perform grid search
gridPerformanceGBM <- h2o.getGrid(
  grid_id = "gbm_grid", 
  sort_by = "mean_per_class_error", 
  decreasing = FALSE
)
gridPerformanceGBM


# Grab the model_id for the top model, chosen by cross validation error
best_model_id <- gridPerformanceGBM@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s get performance metrics on the best model
h2o.performance(model = best_model, xval = TRUE)

#what was the RMSE?
#what was the MSE?

### STACKING MODELS ###
# model
ensembleTree <- h2o.stackedEnsemble(
  x = predictors, y = response, training_frame = trainH2o, model_id = "example_2",
  base_models = list(h2oRF1, h2oGBM),
  metalearner_algorithm = "drf"
)

# evaluation
getMisclass <- function(model) {
  results <- h2o.performance(model, newdata = trainH2o)
  results@metrics$mean_per_class_error
}
list(h2oRF1, h2oGBM) %>%
  purrr::map_dbl(getMisclass)
#[1] 0.7033095 0.7058346
#both random forest and GBM had equal RMSE scores

#final evlauation metric
h2o.performance(ensembleTree, newdata = trainH2o)@metrics$mean_per_class_error
#[1] 0.7018815
#stacked model gave us 0.02 percent performance gain with RMSE of 0.7018815

# Run stacked model on test set

# evaluation
getMisclass <- function(model) {
  results <- h2o.performance(model, newdata = testH2o)
  results@metrics$mean_per_class_error
}
list(h2oRF1, h2oGBM) %>%
  purrr::map_dbl(getMisclass)
#[1] 0.7181616 0.7181616 - GBM and RF perform equally

#final evaluation metric
h2o.performance(ensembleTree, newdata = testH2o)@metrics$mean_per_class_error
#[1] 0.7162188 - improvement from training data!
