library(dplyr)

# your filepath here
path <- "/Users/marinabennett/Desktop/Hertie/1. Fall 2019/Machine Learning/Hertie-ML-TADA-Project/newspaper-data/English"

# create list of all outlets
filenamesList <- list.files(path = path, full.names = TRUE)

# load all .Rdata files
for (i in 1:length(filenamesList)) {
  load(filenamesList[i])
}

# make sure columns match
## dropping columns that are only in some but not all, except topic_tags
## will just add a topic_tags column to the two that are missing: Fox News and Infowars
bbcnews_df <- bbcnews_df %>% select(-section)
politico_df <- politico_df %>% select(-section)
thehill_df <- thehill_df %>% select(-section)
usatoday_df <- usatoday_df %>% select(-section)
washingtonpost_df <- washingtonpost_df %>% select(-section, -subsection)
wsj_df <- wsj_df %>% select(-section, -paywall)
cnbc_df <- cnbc_df %>% select(-news_keywords)
foxnews_df$topic_tags <- "NA"
infowars_df$topic_tags <- "NA"


# rbind
## This would be better if we could automatically create the list of all the dataframes vs. typing them out
fullDataSet <- do.call("rbind", list(abcnews_df,
                                     bbcnews_df,
                                     bloomberg_df,
                                     breitbart_df,
                                     buzzfeed_df,
                                     cnbc_df,
                                     dailykos_df,
                                     foxnews_df,
                                     guardian_df,
                                     huffingtonpost_df,
                                     infowars_df,
                                     motherjones_df,
                                     newsweek_df,
                                     newyorker_df,
                                     nytimes_df,
                                     politico_df,
                                     thehill_df,
                                     thinkprogress_df,
                                     townhall_df,
                                     usatoday_df,
                                     vice_df,
                                     washingtonpost_df,
                                     wsj_df))

