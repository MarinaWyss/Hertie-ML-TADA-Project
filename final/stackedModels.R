### Machine Learning Application of Demographic data on Gun Violence in U.S. ####
library(tidyverse)
library(caret)
library(rsample)
library(randomForest)
library(xgboost)
library(h2o)   

# h2o set-up 
#h2o.no_progress()  # turn off h2o progress bars
#h2o.init()         # launch h2o

# your filepath here
path <- "/Users/madelinebrady/Documents/GitHub/Hertie-ML-TADA-Project/final"

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

#delete gamma from the dataset
joinedDataSet$gamma <- NULL

#downsampling the data 

balancedData <- upSample(x = joinedDataSet[, -1],
                           y = joinedDataSet$topic)

#split data into training and test 
#using simple random sampling using caret package

set.seed(123)  # for reproducibility
index_3 <- createDataPartition(balancedData$topic, p = 0.7, 
                               list = FALSE)
train.1 <- joinedDataSet[index_3, ]
test.1  <- joinedDataSet[-index_3, ]

# view the first 6 rows of the downsampled, training data
head(train.1)

##document topic     gamma    outlet       date          location   zip
#1    text2     1 0.3667261 breitbart 2018-10-01 Chicago, Illinois 60632
#2   text78     1 0.4987770 breitbart 2018-10-01 Chicago, Illinois 60632
#4   text41     2 0.9208401 breitbart 2018-10-01 Chicago, Illinois 60632
#6 text4633     2 0.2815031   foxnews 2018-10-01 Chicago, Illinois 60632
#7 text4636     2 0.8156081   foxnews 2018-10-01 Chicago, Illinois 60632
#8 text4650     2 0.7551036   foxnews 2018-10-01 Chicago, Illinois 60632
#medianIncome majorityRace countyVote dead injusted total children
#1     3.496508            1          0    2        2     4        0
#2     3.496508            1          0    2        2     4        0
#4     3.496508            1          0    2        2     4        0
#6     3.496508            1          0    2        2     4        0
#7     3.496508            1          0    2        2     4        0
#8     3.496508            1          0    2        2     4        0
#murderSuicide
#1             0
#2             0
#4             0
#6             0
#7             0
#8             0

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


# xgboost


set.seed(123)
ames_xgb <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "reg:linear",
  early_stopping_rounds = 50, 
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)  

# minimum test CV RMSE
min(ames_xgb$evaluation_log$test_rmse_mean)



