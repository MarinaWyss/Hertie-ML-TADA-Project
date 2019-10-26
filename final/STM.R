# load packages
library(tidyverse)
library(stringr)
library(stringi)
library(tidyr)
library(quanteda)
library(stm)

# merge dataset
breitbart_df$text <- as.character(breitbart_df$text)
foxnews_df$text <- as.character(foxnews_df$text)
nytimes_df$text <- as.character(nytimes_df$text)
thinkprogress_df$text <- as.character(thinkprogress_df$text)
wsj_df$text <- as.character(wsj_df$text)

wsj_df <- wsj_df %>% select(-section, -paywall)
foxnews_df$topic_tags <- "NA"

fullDataSet <-  rbind(breitbart_df, foxnews_df, nytimes_df, thinkprogress_df, wsj_df)

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
  filter(date >= "2018:10:01" & date <= "2018:11:01")

# create variables for day of the year 
fullDataSet$date <- str_replace_all(fullDataSet$date, ":", "-")
fullDataSet$date <- as.Date(fullDataSet$date)

fullDataSet <- fullDataSet %>% 
  mutate(dayOfYear = strftime(date, format = "%j"))

# add column with outlet and date
fullDataSet$outlet_date = stri_join(fullDataSet$outlet,fullDataSet$date,sep="_")

# create a corpus and dfm
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
                                c("said", "like"))

# filtering articles with tokens relating to guns
gunWords <- c("gun", "shooting", "gunman", "shooter", 
              "guns", "second amendment", 
              "2nd amendment", "firearm", "firearms",
              "NRA", "National Rifle Association")

newsTokens <- keep(newsTokens, ~ any(gunWords %in% .x))

# filtering docvars
filteredNames <- names(newsTokens)
rowsToKeep <- as.numeric(substr(filteredNames, start = 5, stop = 100))
fullDataSet$docNumber <- 1:length(fullDataSet$outlet)
filteredDataSet <- subset(fullDataSet, docNumber %in% rowsToKeep)

newsCorpusFiltered <- corpus(filteredDataSet) 
docvars(newsCorpusFiltered, "outlet_date") <- filteredDataSet$outlet_date


# create DFM

newsDfm <- dfm(newsTokens)

# trim dfm to use in stm function (min 7.5% / max 95%)
newsDfm <- dfm_trim(newsDfm, min_docfreq = 0.075, max_docfreq = 0.90, docfreq_type = "prop") 

# convert quanteda dfm to documents in stm form
newsConvert <- convert(newsDfm, to = "stm", docvars = docvars(newsCorpusFiltered))

# run the stm
topic.count <- 10
newsStm <- stm(newsConvert$documents, 
              newsConvert$vocab, 
              K = topic.count, 
              data = newsConvert$meta, 
              init.type = "Spectral")

data.frame(t(labelTopics(newsStm, n = 10)$prob))

# plot the results
plot(newsStm, type = "perspectives", topics = c(0, 2))



# find the ideal number of topics


# estimate topic relationships
#estimateEffect()



