# load packages
library(tidyverse)

# adding independent variables to data set
fullDataSet <- read.csv("preppedDataSet.csv")
demographics <- read.csv("demoData.csv")

fullDataSet$date <- as.Date(fullDataSet$date)

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



### Naive Bayes practice ###
practiceData <- filteredDataSet %>% 
  select(text, outlet)

practiceDataCorpus <- corpus(practiceData, text_field = "text") 
docvars(practiceDataCorpus, "outlet") <- practiceData$outlet

smp <- sample(c("train", "test"), size = ndoc(practiceDataCorpus), 
              prob = c(0.70, 0.30), replace = TRUE)
train <- which(smp == "train")
test <- which(smp =="test")

characters <- tokens(practiceDataCorpus, what = "word")
namesdfm <- dfm(characters)

nb <- textmodel_nb(namesdfm[train, ], docvars(practiceDataCorpus, "outlet")[train])
preds <- predict(nb, newdata = namesdfm[test, ])
cm <- table(preds, docvars(practiceDataCorpus, "outlet")[test])

precrecall <- function(mytable, verbose=TRUE) {
  truePositives <- mytable[1,1]
  falsePositives <- sum(mytable[1,]) - truePositives
  falseNegatives <- sum(mytable[,1]) - truePositives
  precision <- truePositives / (truePositives + falsePositives)
  recall <- truePositives / (truePositives + falseNegatives)
  if (verbose) {
    print(mytable)
    cat("\n precision =", round(precision, 2), 
        "\n    recall =", round(recall, 2), "\n")
  }
  invisible(c(precision, recall))
}

# precision and recall
precrecall(cm)
# accuracy
sum(diag(cm)) / sum(cm)


# plot
practiceData$docNumber <- 1:length(practiceData$outlet)
practiceData <- subset(practiceData, docNumber %in% test)
practiceData$pred <- predict(nb, newdata = namesdfm[test, ])

plot <- ggplot(aes(x = outlet, y = pred), data =  practiceData) +
  geom_bar(aes(fill = pred), stat = "identity") +
  theme(axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_blank()) +
  labs(fill = "Predicted Outlet") +
  ylab("Count") +
  xlab("Outlet")

plot




