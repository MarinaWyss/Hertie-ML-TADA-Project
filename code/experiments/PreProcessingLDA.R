library(dplyr)
library(tools)
library(stringr)
library(quanteda)
library(topicmodels)
library(tidytext)
library(tidyr)

# your filepath here
path <- "/Users/madelinebrady/Desktop/Fall 2019/Hertie-ML-TADA-Project/newspaper-data/English"

# create list of all outlets
filenamesList <- list.files(path = path, full.names = TRUE)

# load all .Rdata files
for (i in 1:length(filenamesList)) {
  load(filenamesList[i])
}

# creating a character vector of the dataframe names
newsNames <- file_path_sans_ext(basename(filenamesList))
newsNames <- word(newsNames, 1, sep = "_")
newsNames <- paste0(newsNames, "_df")

# rbind
## make sure columns match
### dropping columns that are only in some but not all, except topic_tags
### will just add a topic_tags column to the two that are missing: Fox News and Infowars
bbcnews_df <- bbcnews_df %>% select(-section)
politico_df <- politico_df %>% select(-section)
thehill_df <- thehill_df %>% select(-section)
usatoday_df <- usatoday_df %>% select(-section)
washingtonpost_df <- washingtonpost_df %>% select(-section, -subsection)
wsj_df <- wsj_df %>% select(-section, -paywall)
cnbc_df <- cnbc_df %>% select(-news_keywords)
foxnews_df$topic_tags <- "NA"
infowars_df$topic_tags <- "NA"

fullDataSet <- do.call("rbind", lapply(newsNames, get))

# prepare the data
## split the datetime column into two
fullDataSet$time <- format(as.POSIXct(fullDataSet$datetime, format="%Y:%m:%d %H:%M:%S"), "%H:%M:%S")
fullDataSet$date <- format(as.POSIXct(fullDataSet$datetime, format="%Y:%m:%d %H:%M:%S"), "%Y:%m:%d")
fullDataSet <- fullDataSet %>% select(-datetime)
fullDataSet = fullDataSet %>% select(outlet, outlet_url, date, time, url_orig, headline, description, 
                       author, domain, topic_tags, text)

# split data (70/30 - Train/Test)
## set seed for reproducibility
set.seed(123) 
index <- sample(1:nrow(fullDataSet), round(nrow(fullDataSet) * 0.7))
trainDataSet <- fullDataSet[index, ]
testDataSet  <- fullDataSet[-index, ]

# create a corpus using quanteda 
trainCorpus <- corpus(trainDataSet, text_field = "text", metacorpus = NULL, compress = FALSE)

#create a document feature matrix
## tokenize by word, pre-process, create dfm
trainTokens <- trainCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

trainTokens <- tokens_select(trainTokens, 
                             stopwords('english'), 
                             selection = 'remove')

trainTokens <- tokens_wordstem(trainTokens)

trainDfm <- dfm(trainTokens)

## version with sentences
trainTokens <- trainCorpus %>%
  tokens(what = "sentence", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

trainTokens <- tokens_select(trainTokens, 
                             stopwords('english'), 
                             selection = 'remove')

trainDfmSentence <- dfm(trainTokens)

# Create an LDA object (without a control argument)
trainLDA <- LDA(trainDfm, k = 15, control = list(seed = 1234))

# Word-topic probabilities analysis of LDA object
## Extract per-topic-per-word probabilities (Beta) using tidytext package
trainTopics <- tidy(trainLDA, matrix = "beta")

### Find the 10 terms that are most common within each topic and vizualize
topicsTopTerms <- trainTopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topicsTopTerms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

### Find terms with greatest difference in beta and vizualize
spreadTopTerms <- trainTopics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# Document-topic probabilities analysis of LDA object
## Extract per-document-per-topic probabilities (Gamma) using tidytext package
trainTopics <- tidy(trainLDA, matrix = "gamma")

### check the most common words within a specific document
tidy(trainDfm) %>%
  filter(document == 6) %>%
  arrange(desc(count))



