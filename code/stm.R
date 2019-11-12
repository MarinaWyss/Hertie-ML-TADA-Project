# load packages
library(tidyverse)
library(stringr)
library(stringi)
library(tidyr)
library(quanteda)
library(stm)
library(tools)
library(tidytext)
library(wordcloud)


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

ideology <- read.csv("ideology.csv")
fullDataSet <- merge(fullDataSet, ideology)

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
  filter(date >= "2018:06:01" & date <= "2018:07:31")

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

## filtering docvars
filteredNames <- names(newsTokens)
rowsToKeep <- as.numeric(substr(filteredNames, start = 5, stop = 100))
fullDataSet$docNumber <- 1:length(fullDataSet$outlet)
filteredDataSet <- subset(fullDataSet, docNumber %in% rowsToKeep)

newsCorpusFiltered <- corpus(filteredDataSet) 
docvars(newsCorpusFiltered, "outlet_date") <- filteredDataSet$outlet_date
docvars(newsCorpusFiltered, "ideology") <- filteredDataSet$ideology



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
K <- c(4:8) 
kresult <- searchK(newsConvert$documents, newsConvert$vocab, K, init.type = "Spectral")
plot(kresult)


# run the stm
topic.count <- 6
newsStm <- stm(newsConvert$documents, 
              newsConvert$vocab, 
              K = topic.count, 
              data = newsConvert$meta, 
              init.type = "Spectral")

# view results
data.frame(t(labelTopics(newsStm, n = 20)$prob))

labelTopics(newsStm, c(1:6))

plot(newsStm, type = "summary", topics = c(1:6), xlim = c(0, 6))

# estimate topic relationships
effect <- estimateEffect(formula = 1:20 ~ outlet_date, stmobj = newsStm,
                         metadata = newsConvert$meta, uncertainty = "Global")

summary(effect, topics = 1)


# docvars and topic frequencies
probabilities <- tidy(newsStm, matrix = "gamma", document_names = names(newsTokens))

joinedDataSet <- cbind(probabilities, filteredDataSet$outlet_date)

# save joined dataset
preppedDataSet <- joinedDataSet %>% 
  rename(outlet_date = `filteredDataSet$outlet_date`) %>% 
  separate(outlet_date, into = c("outlet", "date"), sep = "_") %>% 
  group_by(document) %>% 
  filter(gamma == max(gamma)) %>% 
  arrange(date)

write.csv(preppedDataSet, "preppedDataSet.csv")


# words associated with each topic
wordBetas <- tidy(newsStm)

wordBetas %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  mutate(topic = case_when(
    topic == 1 ~ "Police",
    topic == 2 ~ "NationalSecurity",
    topic == 3 ~ "HumanInterest",
    topic == 4 ~ "SecondAmendment",
    topic == 5 ~ "Politics", 
    topic == 6 ~ "SchoolShootings"),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE, color = "black") +
  facet_wrap(~ topic, scales = "free_y") +
  scale_fill_manual(values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest Word Probabilities for Each Topic")




# word cloud for each topic
cloud(newsStm, topic = 1, scale = c(2, 0.5))
cloud(newsStm, topic = 2, scale = c(2, 0.5))
cloud(newsStm, topic = 3, scale = c(2, 0.5))
cloud(newsStm, topic = 4, scale = c(2, 0.5))
cloud(newsStm, topic = 5, scale = c(2, 0.5))
cloud(newsStm, topic = 6, scale = c(2, 0.5))

# distribution of document probabilities for each topic (gamma is the probability that a document belongs in a topic)
## code from https://juliasilge.com/blog/sherlock-holmes-stm/
## becuase we have so many articles, this is not very valuable..shows that each topic is associated w/ 500-2000 articles
probabilities <- tidy(newsStm, matrix = "gamma", document_names = names(newsTokens))

ggplot(probabilities, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       y = "Number of Articles", x = expression(gamma))

## Metadata/topic relation vizualization
# estimate topic relationships
effect <- estimateEffect(formula = 1:6 ~ ideology, stmobj = newsStm,
                         metadata = newsConvert$meta, uncertainty = "Global")
summary(effect)
  # topic 1: a 1 unit increase in idelogy leads to a .07 increase probability that a document is included in topic 1
  # topic 2: a 1 unit increase in ideology leads to a decreased chance that the document is included in this topic
  # topic 3: a 1 unit increase in ideology leads to a decreased chance that the doucment is included in this topic
  # topic 4: no significance
  # topic 5: a 1 unit increase in ideology leads to a decreased chance that the document is included in this topic
  # topic 6: a 1 unit increase in ideology leads to an increased chance that the document is included in this topic

# expected topic proportions per topic
## the expected proportion of a document that belongs to a topic as a function of a covariate,
  # or a first difference type estimate, where topic prevalence for a particular topic is contrasted for two groups 
  # (e.g., liberal versus conservative)
plot(newsStm, type = "summary", xlim = c(0, 0.5))

# plot effect of liberal versus conservative - topical prevelence contrast
plot(effect, covariate = "ideology", topics = c(1:6),
        model = newsStm, method = "difference", cov.value1 = 1,
        cov.value2 = 5,
        xlab = "More Conservative ... More Liberal",
        main = "Effect of Liberal vs. Conservative", xlim = c(-0.4, 0.4),
        labeltype = "custom", custom.labels = c("Police", "National Security",
                                          "Human Interest", "Second Amendment", "Political", "School Shooting"))

# topical content plot - comparing 2 topics
## just comparing 1 and 3 becuase 1 is furthest left and 3 is furthest right
plot(newsStm, type = "perspectives", topics = c(1, 3))

# topical content plot - comparing 2 sides within singular topic
newsContent <- stm(newsConvert$documents, newsConvert$vocab, K = topic.count,
                       prevalence =~ ideology, content =~ ideology,
                       max.em.its = 75, data = newsConvert$meta, init.type = "Spectral")

plot(newsContent, type = "perspectives", topics = c(1))
plot(newsContent, type = "perspectives", topics = c(2))
plot(newsContent, type = "perspectives", topics = c(3))
plot(newsContent, type = "perspectives", topics = c(4))
plot(newsContent, type = "perspectives", topics = c(5))
plot(newsContent, type = "perspectives", topics = c(6))


