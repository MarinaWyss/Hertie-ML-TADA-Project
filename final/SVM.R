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

# downsampling the data 
balancedData <- downSample(x = joinedDataSet[, -1],
                           y = joinedDataSet$topic)

# dropping not needed variables
str(trainDataNews)
balancedData <- balancedData %>% 
  select(-gamma)

# split data into training and test 
set.seed(123)  
index <- createDataPartition(balancedData$topic, p = 0.7, 
                             list = FALSE)
trainDataNews <- balancedData[index, ]
testDataNews  <- balancedData[-index, ]

#-----------------------------------------------
## svm for multiple classes
# packages not used
library(svmpath)
library(pdp)
library(vip)
library(ranger)
library(kernlab)
library(RColorBrewer)

# load packages
library(e1071)

## fit model using e1071 package
svmfit <- svm(topic~., data = trainDataNews, kernel = "radial", cost = 10, gamma = 1)

#construct table to check model fit
ypred <- predict(svmfit, trainDataNews)
(misclass <- table(predict = ypred, truth = trainDataNews$topic))
# 56% accuracy for train data

# plot results (only works with 2 continuous x variables at a time)
# plot(svmfit, trainDataNews)

# validate by using train data
## fit model using e1071 package
svmfitTest <- svm(topic~., data = testDataNews, kernel = "radial", cost = 10, gamma = 1)

#construct table to check model fit
ypredTest <- predict(svmfitTest, testDataNews)
(misclassTest <- table(predict = ypredTest, truth = testDataNews$topic))
# 24% accuraacy on test data

####################
library(kernlab)

?ksvm

ksvmNews <- ksvm(topic ~ ., data = trainDataNews, type = "kbb-svc", kernel = "rbfdot", C = 0.5, prob.model = TRUE)

ksvmNews