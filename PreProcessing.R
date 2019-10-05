library(dplyr)
library(tools)
library(stringr)

# your filepath here
path <- "/Users/marinabennett/Desktop/Hertie/1. Fall 2019/Machine Learning/Hertie-ML-TADA-Project/newspaper-data/English"

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

