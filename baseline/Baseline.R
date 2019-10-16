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
wsjBaseline$topic <- tolower(wsjBaseline$topic)
nyTimesBaseline$text <- as.character(nyTimesBaseline$text)
nyTimesBaseline$topic <- tolower(nyTimesBaseline$topic)



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

trainWSJ <- which(wsjsmp == "train")
testWSJ <- which(wsjsmp == "test")


# NY Times pre-processing
nyTimesCorpus <- corpus(nyTimesBaseline, text_field = "text", metacorpus = NULL, compress = FALSE)
docvars(nyTimesCorpus, "topic") <- nyTimesBaseline$topic

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

trainNYT <- which(nytsmp == "train")
testNYT <- which(nytsmp == "test")



# training Naive Bayes models
wsjNB <- textmodel_nb(wsjDfm[trainWSJ, ], docvars(wsjCorpus, "topic")[trainWSJ])
wsjPreds <- data.frame(predict(wsjNB, newdata = wsjDfm[testWSJ,]))

nyTimesNB <- textmodel_nb(nyTimesDfm[trainNYT,], docvars(nyTimesCorpus, "topic")[trainNYT])
nyTimesPreds <- data.frame(predict(nyTimesNB, newdata = nyTimesDfm[testNYT,]))


# testing
trueTestWSJ <- (wsjBaseline[testWSJ, ])
trueTestWSJ <- data.frame(trueTestWSJ$topic)
trueTestWSJ$prediction <- wsjPreds$predict.wsjNB..newdata...wsjDfm.testWSJ....
levels(trueTestWSJ$prediction) <- levels(trueTestWSJ$trueTestWSJ.topic)
trueTestWSJ$accurate <- ifelse(trueTestWSJ$trueTestWSJ.topic == trueTestWSJ$prediction, 1, 0)

## accuracy WSJ
accuracyWSJ <- (sum(trueTestWSJ$accurate) / nrow(trueTestWSJ))


trueTestNYT <- (nyTimesBaseline[testNYT, ])
trueTestNYT <- data.frame(trueTestNYT$topic)
trueTestNYT$prediction <- nyTimesPreds$predict.nyTimesNB..newdata...nyTimesDfm.testNYT....
levels(trueTestNYT$prediction) <- levels(trueTestNYT$trueTestNYT.topic)
trueTestNYT$accurate <- ifelse(trueTestNYT$trueTestNYT.topic == trueTestNYT$prediction, 1, 0)

## accuracy NYT
accuracyNYT <- (sum(trueTestNYT$accurate) / nrow(trueTestNYT))

# distribution of topics

## WSJ 
wsjBefore <- wsjBaseline %>%
  filter(date == "2018:05:07") 

wsjAfter <- wsjBaseline %>%
  filter(date == "2018:05:08") 

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

wsjProp <- merge(wsjBeforeProp, wsjAfterProp, by = "topic", all = TRUE)
wsjProp$change <- wsjProp$propAfter - wsjProp$propBefore


# plots
wsjBeforePlot <- ggplot(aes(x = topic, y = propBefore), data = wsjProp) +
  geom_col(aes(fill = topic), color = "white") +
  ylim(0, 0.35) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 18, family="Times")) +
  labs(x = "Topics on May 7th, 2018", 
       y = "Proportion out of total topics")


wsjAfterPlot <- ggplot(aes(x = topic, y = propAfter), data = wsjProp) +
  geom_col(aes(fill = topic), color = "white") +
  ylim(0, 0.35) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 18, family="Times")) +
  labs(x = "Topics on May 8th, 2018", 
       y = "Proportion out of total topics")


## NY Times 
nyTimesBefore <- nyTimesBaseline %>%
  filter(date == "2018:05:07") 

nyTimesAfter <- nyTimesBaseline %>%
  filter(date == "2018:05:08") 

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

nyTimesProp <- merge(nyTimesBeforeProp, nyTimesAfterProp, by = "topic", all = TRUE)
nyTimesProp$change <- nyTimesProp$propAfter - nyTimesProp$propBefore

# plots

nyTimesBeforePlot <- ggplot(aes(x = topic, y = propBefore), data = nyTimesProp) +
  geom_col(aes(fill = topic), color = "white") +
  ylim(0, 0.35) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 18, family="Times")) +
  labs(x = "Topics on May 7th, 2018", 
       y = "Proportion out of total topics")

nyTimesAfterPlot <- ggplot(aes(x = topic, y = propAfter), data = nyTimesProp) +
  geom_col(aes(fill = topic), color = "white") +
  ylim(0, 0.35) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 18, family="Times")) +
  labs(x = "Topics on May 8th, 2018", 
       y = "Proportion out of total topics")


## formatting table

propChange <- merge(wsjProp, nyTimesProp, by = "topic", all = TRUE)
propChange[is.na(propChange)] <- 0

fixNums <- function(x){
  x <- x * 100
  x <- round(x, 2)
  return(x)
}

propChange <- propChange %>%
  mutate_if(is.numeric, funs(fixNums))

propChange <- propChange %>%
  select(topic, propBefore.x, propAfter.x, change.x, 
         propBefore.y, propAfter.y, change.y)

names(propChange) <- c("Topic", "BeforeWSJ", "AfterWSJ", "ChangeWSJ", 
                       "BeforeNYT", "AfterNYT", "ChangeNYT")

propChange %>%
  kable %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  column_spec(1, bold = T, border_right = T) %>%
  add_header_above(c(" " = 1, "Wall Street Journal" = 3, "New York Times" = 3)) %>%
  footnote(general = "Proportion of each topic before and after the withdrawal from the Iran Nuclear Deal - May 7th to 8th, 2018")



