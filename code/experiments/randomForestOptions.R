library(caret)
library(rsample)
library(ranger) 
library(h2o)
library(tidyverse)
library(e1071)
library(randomForest)
library(mlbench)

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


## first default random forest model using the caret package

#cross-validaton, with 10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)

#Number randomely variable selected is mtry
mtry <- sqrt(ncol(train.1))
tunegrid <- expand.grid(.mtry=mtry)

rf_default <- train(topic~., 
                    data=train.1, #uses downsampled, split train data
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)


### code from slava
# train a default random forest model 2
default.rf1 <- ranger(
  topic ~ ., 
  data = joinedDataSet,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123
)

# get the Out of Bag RMSE (error)
(default_rmse <- sqrt(default.rf1$prediction.error))

# create 
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),  # split rule
  min.node.size = c(1, 3, 5, 10),                         # tree complexity
  replace = c(TRUE, FALSE),                               # sampling scheme
  sample.fraction = c(.5, .63, .8),                       # sampling scheme
  rmse = NA                                               # results placeholder
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = topic ~ ., 
    data            = joinedDataSet, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# assess top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)


