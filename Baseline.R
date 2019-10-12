library(dplyr)
library(tools)
library(stringr)
library(quanteda)
library(topicmodels)
library(tidytext)
library(tidyr)

# loading small datasets
wsjBaseline <- read.csv("wsjBaseline.csv")
nyTimesBaseline <- read.csv("nyTimesBaseline.csv")

wsjBaseline$text <- as.character(wsjBaseline$text)
nyTimesBaseline$text <- as.character(nyTimesBaseline$text)


# split data (70/30 - Train/Test)
set.seed(123) 

wsjIndex <- sample(1:nrow(wsjBaseline), round(nrow(wsjBaseline) * 0.7))
wsjTrain <- wsjBaseline[wsjIndex, ]
wsjTest  <- wsjBaseline[-wsjIndex, ]

nyTimesIndex <- sample(1:nrow(nyTimesBaseline), round(nrow(nyTimesBaseline) * 0.7))
nyTimesTrain <- nyTimesBaseline[nyTimesIndex, ]
nyTimesTest  <- nyTimesBaseline[-nyTimesIndex, ]


# WSJ pre-processing
## train
wsjTrainCorpus <- corpus(wsjTrain, text_field = "text", metacorpus = NULL, compress = FALSE)
docvars(wsjTrainCorpus, "topic") <- wsjTrain$topic

wsjTrainTokens <- wsjTrainCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

wsjTrainTokens <- tokens_select(wsjTrainTokens, 
                             stopwords('english'), 
                             selection = 'remove')

wsjTrainTokens <- tokens_wordstem(wsjTrainTokens)

wsjTrainDfm <- dfm(wsjTrainTokens)

## test
wsjTestCorpus <- corpus(wsjTest, text_field = "text", metacorpus = NULL, compress = FALSE)
docvars(wsjTestCorpus, "topic") <- wsjTest$topic

wsjTestTokens <- wsjTestCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

wsjTestTokens <- tokens_select(wsjTestTokens, 
                                stopwords('english'), 
                                selection = 'remove')

wsjTestTokens <- tokens_wordstem(wsjTestTokens)

wsjTestDfm <- dfm(wsjTestTokens)


# NY Times pre-processing
## train
nyTimesTrainCorpus <- corpus(nyTimesTrain, text_field = "text", metacorpus = NULL, compress = FALSE)
docvars(nyTimesTrainCorpus, "topic") <- nyTimesTrain$topic

nyTimesTrainTokens <- nyTimesTrainCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

nyTimesTrainTokens <- tokens_select(nyTimesTrainTokens, 
                                stopwords('english'), 
                                selection = 'remove')

nyTimesTrainTokens <- tokens_wordstem(nyTimesTrainTokens)

nyTimesTrainDfm <- dfm(nyTimesTrainTokens)

## test
nyTimesTestCorpus <- corpus(nyTimesTest, text_field = "text", metacorpus = NULL, compress = FALSE)
docvars(nyTimesTestCorpus, "topic") <- nyTimesTest$topic

nyTimesTestTokens <- nyTimesTestCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

nyTimesTestTokens <- tokens_select(nyTimesTestTokens, 
                                    stopwords('english'), 
                                    selection = 'remove')

nyTimesTestTokens <- tokens_wordstem(nyTimesTestTokens)

nyTimesTestDfm <- dfm(nyTimesTestTokens)


# training Naive Bayes models
wsjNB <- textmodel_nb(wsjTrainDfm, docvars(wsjTrainCorpus, "topic"))
wsjPreds <- predict(wsjNB, newdata = wsjTestDfm)

nyTimesNB <- textmodel_nb(nyTimesTrainDfm, docvars(nyTimesTrainCorpus, "topic"))
nyTimesPreds <- predict(nyTimesNB, newdata = nyTimesTestDfm)


# testing
wsjCM <- table(wsjPreds, docvars(wsjTestCorpus, "topic"))
nyTimesCM <- table(nyTimesPreds, docvars(nyTimesTestCorpus, "topic"))

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





