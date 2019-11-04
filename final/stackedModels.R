### Machine Learning Application of Demographic data on Gun Violence in U.S. ####
library(caret)
library(tidyverse)
library(h2o)
h2o.init()

# adding independent variables to data set
fullDataSet <- read.csv("preppedDataSet.csv")
demographics <- read.csv("demoData.csv")

fullDataSet <- fullDataSet %>% 
  mutate(date = as.Date(date),
         topic = as.factor(topic))

demographics <- demographics %>% 
  mutate(date = as.Date(date),
         children = as.factor(ifelse(children == "Yes", 1, 0)),
         murderSuicide = as.factor(ifelse(murderSuicide == "Yes", 1, 0)),
         countyVote = as.factor(ifelse(countyVote == "Yes", 1, 0)),
         majorityRace = as.factor(case_when(
           majorityRace == "White" ~ 1,
           majorityRace == "Black" ~ 2,
           majorityRace == "Latino" ~ 3,
           majorityRace == "Asian" ~ 4,
           majorityRace == "American Indian" ~ 5)),
         medianIncome = as.numeric(medianIncome),
         medianIncome = log(medianIncome),
         zip = as.factor(zip)
  )

joinedDataSet <- left_join(fullDataSet, demographics) %>% 
  fill(c(6:15), .direction = c("down"))
str(joinedDataSet)

# dropping not needed variables
balancedData <- balancedData %>% 
  select(-gamma)

# downsampling the data 
balancedData <- downSample(x = joinedDataSet[, -1],
                           y = joinedDataSet$topic)

# split data into training and test 
set.seed(123)  
index <- createDataPartition(balancedData$topic, p = 0.7, 
                               list = FALSE)
trainDataNews <- balancedData[index, ]
testDataNews  <- balancedData[-index, ]

# stacked models
Y = "topic"
X <- setdiff(names(balancedData), Y)

train_h2o_news <- trainDataNews %>% 
  as.h2o

test_h2o_news <- testDataNews %>% 
  as.h2o

best_xgb_news <- h2o.xgboost(
  x = X, y = Y, training_frame = train_h2o_news, ntrees = 5000, learn_rate = 0.05,
  max_depth = 3, min_rows = 3, sample_rate = 0.8, categorical_encoding = "Enum",
  nfolds = 10, fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE, seed = 123, stopping_rounds = 50,
  stopping_metric = "RMSE", stopping_tolerance = 0
)

best_gbm_news <- h2o.gbm(
  x = X, y = Y, training_frame = train_h2o_news, ntrees = 5000, learn_rate = 0.01,
  max_depth = 7, min_rows = 5, sample_rate = 0.8, nfolds = 10,
  fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
  seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
  stopping_tolerance = 0
)

ensemble_tree_news <- h2o.stackedEnsemble(
  x = X, y = Y, training_frame = train_h2o_news, model_id = "my_tree_ensemble",
  base_models = list(best_gbm, best_xgb),
  metalearner_algorithm = "drf"
)

get_rmse <- function(model) {
  results <- h2o.performance(model, newdata = test_h2o_news)
  results@metrics$RMSE
}
list(best_gbm_news, best_xgb_news) %>%
  purrr::map_dbl(get_rmse)

# Stacked results
h2o.performance(ensemble_tree_news, newdata = test_h2o_news)@metrics$RMSE
