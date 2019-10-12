library(dplyr)
library(tools)
library(stringr)
library(quanteda)
library(topicmodels)
library(tidytext)
library(tidyr)
library(ggplot2)
library(plyr)
library(kableExtra)
library(extrafont)

# loading small datasets
wsjBaseline <- read.csv("wsjBaseline.csv")
nyTimesBaseline <- read.csv("nyTimesBaseline.csv")

wsjBaseline$text <- as.character(wsjBaseline$text)
nyTimesBaseline$text <- as.character(nyTimesBaseline$text)


# WSJ pre-processing
wsjCorpus <- corpus(wsjBaseline, text_field = "text", metacorpus = NULL, compress = FALSE)

docvars(wsjCorpus, "topic") <- wsjBaseline$topic

wsjTokens <- wsjCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

wsjTokens <- tokens_select(wsjTokens, 
                             stopwords('english'), 
                             selection = 'remove')

wsjTokens <- tokens_wordstem(wsjTokens)

wsjDfm <- dfm(wsjTokens)


## split data (70/30 - Train/Test)
set.seed(123) 

wsjsmp <- sample(c("train", "test"), size=ndoc(wsjCorpus), 
                 prob=c(0.70, 0.30), replace=TRUE)

train <- which(wsjsmp == "train")
test <- which(wsjsmp == "test")


# NY Times pre-processing
nyTimesCorpus <- corpus(nyTimesBaseline, text_field = "text", metacorpus = NULL, compress = FALSE)
docvars(nyTimesCorpus, "topic") <- nyTimes$topic

nyTimesTokens <- nyTimesCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

nyTimesTokens <- tokens_select(nyTimesTokens, 
                                stopwords('english'), 
                                selection = 'remove')

nyTimesTokens <- tokens_wordstem(nyTimesTokens)

nyTimesDfm <- dfm(nyTimesTokens)

## split data (70/30 - Train/Test)
set.seed(123) 

nytsmp <- sample(c("train", "test"), size=ndoc(nyTimesCorpus), 
                 prob=c(0.70, 0.30), replace=TRUE)

train <- which(nytsmp == "train")
test <- which(nytsmp == "test")



# training Naive Bayes models
wsjNB <- textmodel_nb(wsjDfm[train,], docvars(wsjCorpus, "topic")[train])
wsjPreds <- predict(wsjNB, newdata = wsjDfm[test,])

nyTimesNB <- textmodel_nb(nyTimesDfm[train,], docvars(nyTimesCorpus, "topic")[train])
nyTimesPreds <- predict(nyTimesNB, newdata = nyTimesDfm[test,])


# testing
wsjCM <- table(wsjPreds, docvars(wsjCorpus, "topic")[test])
nyTimesCM <- table(nyTimesPreds, docvars(nyTimesCorpus, "topic")[test])

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

precrecall(wsjCM)
sum(diag(wsjCM)) / sum(wsjCM)

precrecall(nyTimesCM)
sum(diag(nyTimesCM)) / sum(nyTimesCM)


# distribution of topics

## WSJ 

# plots
wsjBefore <- wsjBaseline %>%
  filter(date == "2018:05:07") 

wsjBeforePlot <- ggplot(aes(x = topic), data = wsjBefore) +
  geom_bar(stat = "count", aes(fill = topic), color = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family="Times"))

wsjAfter <- wsjBaseline %>%
  filter(date == "2018:05:08") 

wsjAfterPlot <- ggplot(aes(x = topic), data = wsjAfter) +
  geom_bar(stat = "count", aes(fill = topic), color = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family="Times"))

# proportions
wsjBeforeProp <- wsjBefore %>%
  group_by(topic) %>%
  tally()

wsjBeforeProp <- wsjBeforeProp %>%
  mutate(propBefore = (n/sum(n))) 

wsjAfterProp <- wsjAfter %>%
  group_by(topic) %>%
  tally()

wsjAfterProp <- wsjAfterProp %>%
  mutate(propAfter = (n/sum(n)))

wsjProp <- merge(wsjBeforeProp, wsjAfterProp, by = "topic")
wsjProp$change <- wsjProp$propAfter - wsjProp$propBefore

wsjProp %>%
  select(topic, propBefore, propAfter, change) %>%
  kable %>%
  kable_styling()


## NY Times 

# plots
nyTimesBefore <- nyTimesBaseline %>%
  filter(date == "2018:05:07") 

nyTimesBeforePlot <- ggplot(aes(x = topic), data = nyTimesBefore) +
  geom_bar(stat = "count", aes(fill = topic), color = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family="Times"))

nyTimesAfter <- nyTimesBaseline %>%
  filter(date == "2018:05:08") 

nyTimesAfterPlot <- ggplot(aes(x = topic), data = nyTimesAfter) +
  geom_bar(stat = "count", aes(fill = topic), color = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family="Times"))

# proportions
nyTimesBeforeProp <- nyTimesBefore %>%
  group_by(topic) %>%
  tally()

nyTimesBeforeProp <- nyTimesBeforeProp %>%
  mutate(propBefore = (n/sum(n))) 

nyTimesAfterProp <- nyTimesAfter %>%
  group_by(topic) %>%
  tally()

nyTimesAfterProp <- nyTimesAfterProp %>%
  mutate(propAfter = (n/sum(n)))

nyTimesProp <- merge(nyTimesBeforeProp, nyTimesAfterProp, by = "topic")
nyTimesProp$change <- nyTimesProp$propAfter - nyTimesProp$propBefore

nyTimesProp %>%
  select(topic, propBefore, propAfter, change) %>%
  kable %>%
  kable_styling()