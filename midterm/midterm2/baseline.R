library(tidyverse)
library(tools)
library(quanteda)
library(topicmodels)
library(tidytext)
library(plyr)

gunWords <- c("gun", "firearm", "shooting", "gunman", "shooter", 
              "guns", "second amendment", 
              "2nd amendment", "firearm", "firearms",
              "NRA", "National Rifle Association")

politico_df$text <- as.character(politico_df$text)

# WSJ pre-processing
politicoCorpus <- corpus(politico_df, text_field = "text", metacorpus = NULL, compress = FALSE)

docvars(politicoCorpus, "topic") <- politico_df$topic

politicoTokens <- politicoCorpus %>%
  tokens(what = "word", 
         remove_url = TRUE, 
         remove_punct = TRUE, 
         remove_separators = TRUE, 
         remove_numbers = TRUE)

politicoTokens <- tokens_select(politicoTokens, 
                           stopwords('english'), 
                           selection = 'remove')


politicoTokens <- keep(politicoTokens, ~ any(gunWords %in% .x))





