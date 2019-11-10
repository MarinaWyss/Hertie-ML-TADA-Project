# load packages
library(tidyverse)
library(stringr)
library(stringi)
library(tidyr)
library(quanteda)
library(stm)
library(tools)
library(tidytext)

# your filepath here
path <- "/Users/marinabennett/Desktop/Hertie/1. Fall 2019/Machine Learning/Hertie-ML-TADA-Project/newspaper-data/English/finalFiles"

# load data
## create list of all outlets
filenamesList <- list.files(path = path, full.names = TRUE)

## load all .Rdata files
for (i in 1:length(filenamesList)) {
  load(filenamesList[i])
}

## creating a character vector of the dataframe names
newsNames <- file_path_sans_ext(basename(filenamesList))
newsNames <- word(newsNames, 1, sep = "_")
newsNames <- paste0(newsNames, "_df")


# merge dataset
politico_df <- politico_df %>% select(-section)
thehill_df <- thehill_df %>% select(-section)
usatoday_df <- usatoday_df %>% select(-section)
washingtonpost_df <- washingtonpost_df %>% select(-section, -subsection)
wsj_df <- wsj_df %>% select(-section, -paywall)
cnbc_df <- cnbc_df %>% select(-news_keywords)
foxnews_df$topic_tags <- "NA"
infowars_df$topic_tags <- "NA"

fullDataSet <- do.call("rbind", lapply(newsNames, get))

# filter for sports
fullDataSet <- fullDataSet %>%
  filter(domain != "Sports" & 
           domain != "World Cup" &
           domain != "Pro Basketball" &
           domain != "Baseball" &
           domain != "Soccer" & 
           domain != "Tennis" &
           domain != "Pro Football")


# pre-processing
## split the datetime column into two
fullDataSet$time <- format(as.POSIXct(fullDataSet$datetime, format="%Y:%m:%d %H:%M:%S"), "%H:%M:%S")
fullDataSet$date <- format(as.POSIXct(fullDataSet$datetime, format="%Y:%m:%d %H:%M:%S"), "%Y:%m:%d")
fullDataSet <- fullDataSet %>% select(-datetime)

## limit time period for baseline
fullDataSet <- fullDataSet %>%
  filter(date >= "2018:10:01" & date <= "2018:11:30")

## create variables for day of the year 
fullDataSet$date <- str_replace_all(fullDataSet$date, ":", "-")
fullDataSet$date <- as.Date(fullDataSet$date)

fullDataSet <- fullDataSet %>% 
  mutate(dayOfYear = strftime(date, format = "%j"))

## add column with outlet and date
fullDataSet$outlet_date = stri_join(fullDataSet$outlet, fullDataSet$date,sep="_")

## create a corpus and dfm
newsCorpus <- corpus(fullDataSet) 


## tokenize by word, pre-process, create dfm
newsTokens <- newsCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

newsTokens <- tokens_remove(newsTokens,
                                stopwords('english'), 
                                min_nchar = 3L)

newsTokens <- tokens_remove(newsTokens,
                                c("said", "say", "says", "like", 
                                  "p.m.", "a.m.", "don?", "it?", 
                                  "breitbart", "times", "york",
                                  "pic.twitter.com"))

## filtering articles with tokens relating to guns
gunWords <- c("gun", "shooting", "gunman", "shooter", 
              "guns", "second amendment", "shoot", 
              "2nd amendment", "firearm", "firearms",
              "NRA", "National Rifle Association")

newsTokens <- keep(newsTokens, ~ any(gunWords %in% .x))

## manually filtered tokens for only the relevant documents
filteredNames <- names(newsTokens)
allGunTokens <- as.numeric(substr(filteredNames, start = 5, stop = 100))

### saved out this full data set, did manual filter, re-load
relevantArticles <- read.csv("filteredArticles.csv")
tokensKeep <- relevantArticles$doc
fullDataSet$docNumber <- 1:length(fullDataSet$outlet)
filteredDataSet <- subset(fullDataSet, docNumber %in% tokensKeep)

### filter docvars
newsCorpusFiltered <- corpus(filteredDataSet) 
docvars(newsCorpusFiltered, "outlet_date") <- filteredDataSet$outlet_date

### create new corpus etc.
newsCorpus <- corpus(filteredDataSet) 

newsTokens <- newsCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

newsTokens <- tokens_remove(newsTokens,
                            stopwords('english'), 
                            min_nchar = 3L)

# create DFM
newsDfm <- dfm(newsTokens)

## trim dfm to use in stm function (min 7.5% / max 95%)
newsDfm <- dfm_trim(newsDfm, 
                    min_docfreq = 0.075, 
                    max_docfreq = 0.90, 
                    docfreq_type = "prop") 


# convert quanteda dfm to documents in stm form
newsConvert <- convert(newsDfm, to = "stm", 
                       docvars = docvars(newsCorpusFiltered))


# find the ideal number of topics
set.seed(123)
K <- c(4:15) 
kresult <- searchK(newsConvert$documents, newsConvert$vocab, K, init.type = "Spectral")
plot(kresult)


# run the stm
topic.count <- 5

newsStm <- stm(newsConvert$documents, 
              newsConvert$vocab, 
              K = topic.count, 
              data = newsConvert$meta, 
              init.type = "Spectral")


# view results
data.frame(t(labelTopics(newsStm, n = 20)$prob))

labelTopics(newsStm, c(1:5))

plot(newsStm, type = "summary", topics = c(1:5), xlim = c(0, 4))


# estimate topic relationships
effect <- estimateEffect(formula = 1:20 ~ outlet_date, stmobj = newsStm,
                         metadata = newsConvert$meta, uncertainty = "Global")

summary(effect, topics = 1)


# docvars and topic frequencies
probabilities <- tidy(newsStm, matrix = "gamma", document_names = names(newsTokens))

joinedDataSet <- cbind(probabilities, filteredDataSet$outlet_date)

preppedDataSet <- joinedDataSet %>% 
  rename(outlet_date = `filteredDataSet$outlet_date`) %>% 
  separate(outlet_date, into = c("outlet", "date"), sep = "_") %>% 
  group_by(document) %>% 
  filter(gamma == max(gamma)) %>% 
  arrange(date)

write.csv(preppedDataSet, "preppedDataSet.csv")


