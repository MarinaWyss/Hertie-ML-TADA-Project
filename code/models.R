library(caret)
library(h2o)
library(tidyverse)
library(kableExtra)

h2o.init()   

set.seed(123)  

data <- read.csv("preppedDataSet.csv")
ideology <- read.csv("ideology.csv")
data <- merge(data, ideology)
data$topic <- as.factor(data$topic)
data$X <- NULL

# upsampling the data 
data <- upSample(x = data,
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
##OOB RMSE is: 0.7896823
##OOB MSE is: 0.6235981
##OOB Mean Per-Class Error: 0.6707022

# hyperparameter tuning to search a larger grid space for best possible model
hyperGridRF <- list(
  mtries = c(-1, 1, 2, 3),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

## set a random grid search to determine time constraints for finding best model
searchCriteriaRF <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "misclassification",
  stopping_tolerance = 0.001,   # stop if we don't experience 0.1% improvement
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*10      # or cut grid search off at 10 minutes
)

## perform grid search
randomGridRF <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
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

## collect the results and sort by our model performance metric 
gridPerformanceRF <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mean_per_class_error", 
  decreasing = FALSE
)

gridPerformanceRF
# the grid assessed 66 models before stopping 
# best model acheived mean per class error of:  0.6724623223850393 - around 67 percent
# best model has 20 max depth, 3.0 minimum rows, -1 mtries, 0.8 sample rate


## adapting our RF to best hyperparameters
h2oRF2 <- h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = trainH2o, 
  ntrees = 1000, 
  max_depth = 20, 
  min_rows = 3, 
  sample_rate = 0.8, 
  nfolds = 10,
  fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE,
  seed = 123, 
  stopping_rounds = 50, 
  stopping_metric = "misclassification",
  stopping_tolerance = 0
)

h2oRF2
# mean per class error =  0.6707022 - no change


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

# hyperparameter tuning
hyperGridGBM <- list(
  max_depth = c(1, 3, 5),
  min_rows = c(1, 5, 10),
  learn_rate = c(0.01, 0.05, 0.1),
  learn_rate_annealing = c(0.99, 1),
  sample_rate = c(0.5, 0.75, 1),
  col_sample_rate = c(0.8, 0.9, 1)
)

## random grid search strategy
searchCriteriaGBM <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "misclassification",
  stopping_tolerance = 0.001,   
  stopping_rounds = 10,         
  max_runtime_secs = 60*60    #one hour  
)

## perform grid search
randomGridGBM <- h2o.grid(
  algorithm = "gbm", 
  grid_id = "gbm_grid_2", 
  x = predictors, 
  y = response, 
  training_frame = trainH2o,  
  hyper_params = hyperGridGBM,
  search_criteria = searchCriteriaGBM, 
  ntrees = 5000, 
  stopping_metric = "misclassification",     
  stopping_rounds = 10, 
  stopping_tolerance = 0, 
  nfolds = 10, 
  fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE,
  seed = 123
)

## collect the results and sort by our model performance metric 
gridPerformanceGBM <- h2o.getGrid(
  grid_id = "gbm_grid_2", 
  sort_by = "mean_per_class_error", 
  decreasing = FALSE
)

gridPerformanceGBM
# the grid assessed 74 models before stopping 
# best model acheived mean per class error of: 0.6868442292171105
# best model has 3 max depth, 5 minimum rows, 0.8 col_sample_rate, 0.1 learn_rate, 0.5 sample rate

## adapting to best hyperparameters
h2oGBM2 <- h2o.gbm(
  x = predictors, 
  y = response, 
  training_frame = trainH2o, 
  ntrees = 500, 
  learn_rate = 0.1,
  max_depth = 3, 
  min_rows = 5, 
  sample_rate = 0.5, 
  nfolds = 10,
  fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE,
  seed = 123, 
  stopping_rounds = 50, 
  stopping_metric = "misclassification",
  stopping_tolerance = 0
)

h2oGBM2
# mean per class error is 0.667



### STACKING MODELS ###

## with OOB models
ensembleTree <- h2o.stackedEnsemble(
  x = predictors, 
  y = response, 
  training_frame = trainH2o,
  base_models = list(h2oRF1, h2oGBM),
  metalearner_algorithm = "drf"
)

h2o.performance(ensembleTree, newdata = trainH2o)
# mean per class error = 0.6836158


## with tuned models
ensembleTree2 <- h2o.stackedEnsemble(
  x = predictors, 
  y = response, 
  training_frame = trainH2o,
  base_models = list(h2oRF2, h2oGBM2),
  metalearner_algorithm = "drf"
)

h2o.performance(ensembleTree2, newdata = trainH2o)
# mean per class error = 0.6721146


## on test data
h2o.performance(ensembleTree2, newdata = testH2o)
# mean per class error = 0.6741996


# table
confusionMatrix <- h2o.confusionMatrix(ensembleTree2, newdata = testH2o)

names(confusionMatrix) <- c("Police", "NationalSecurity", "HumanInterest",
                            "SecondAmendment", "Politics","SchoolShootings",
                            "Error", "Rate")

rownames(confusionMatrix) <- c("Police", "NationalSecurity", "HumanInterest",
                               "SecondAmendment", "Politics","SchoolShootings",
                               "Totals")

confusionMatrix %>% 
  kable() %>% 
  kable_styling() %>% 
  column_spec(1, bold = T, border_right = T) %>%
  add_header_above(c(" " = 1, "Ensemble Model Results: Confusion Matrix" = 8)) %>% 
  footnote("Rows: Actual class, Columns: Predicted class")


