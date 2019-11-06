library(tidyverse)
library(tools)
library(quanteda)
library(topicmodels)
library(tidytext)
library(plyr)


# load data
breitbart_df$text <- as.character(breitbart_df$text)
foxnews_df$text <- as.character(foxnews_df$text)
nytimes_df$text <- as.character(nytimes_df$text)
thinkprogress_df$text <- as.character(thinkprogress_df$text)
wsj_df$text <- as.character(wsj_df$text)

wsj_df <- wsj_df %>% select(-section, -paywall)
foxnews_df$topic_tags <- "NA"

fullDataSet <- rbind(breitbart_df, foxnews_df, nytimes_df, thinkprogress_df, wsj_df)

# filtering for sports
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

# limiting time period for baseline
fullDataSet <- fullDataSet %>%
  filter(date >= "2018:10:26" & date <= "2018:11:12")

# create a corpus 
baselineCorpus <- corpus(fullDataSet, text_field = "text", metacorpus = NULL, compress = FALSE)

#create a document feature matrix
## tokenize by word, pre-process, create dfm
baselineTokens <- baselineCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

baselineTokens <- tokens_remove(baselineTokens,
                             stopwords('english'), 
                             min_nchar = 3L)

baselineTokens <- tokens_remove(baselineTokens,
                                c("said", "like"))

# filtering articles with tokens relating to guns
gunWords <- c("gun", "shooting", "gunman", "shooter", 
              "guns", "second amendment", 
              "2nd amendment", "firearm", "firearms",
              "NRA", "National Rifle Association")

baselineTokens <- keep(baselineTokens, ~ any(gunWords %in% .x))

# create Dfm
baselineDfm <- dfm(baselineTokens)

# Create an LDA object (without a control argument)
LDA10 <- LDA(baselineDfm, k = 10, control = list(seed = 1234))
LDA15 <- LDA(baselineDfm, k = 15, control = list(seed = 1234))


# Word-topic probabilities analysis of LDA object
## Extract per-topic-per-word probabilities (Beta) using tidytext package
baselineTopics <- tidy(LDA10, matrix = "beta")

### Find the 10 terms that are most common within each topic and vizualize
topicsTopTerms <- baselineTopics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topicsTopTerms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

