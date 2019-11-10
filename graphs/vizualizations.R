library(caret)
library(h2o)
library(tidyverse)
library(vip)
library(wesanderson)

h2o.init()   

set.seed(123)  

joinedDataSet <- read.csv("joinedDataSet.csv")
joinedDataSet$topic <- as.factor(joinedDataSet$topic)

# vizualizations

## overall top topic by outlet (before downsampling)
## calculate topic proportion for each outlet
# breitbart: top topic is #1
breitbartData <- joinedDataSet %>% filter(outlet == "breitbart")

?scale_fill_manual

ggplot(breitbartData, aes(x=factor(topic)))+
  geom_bar(stat="count", width=0.7, fill = )

breitbartCount <- as.data.frame(table(breitbartData$topic))
breitbartCount %>% mutate(n = length(breitbartData$topic),
                          topic_proportion = Freq / n
                            )

breitbartProp <- max(breitbartCount$Freq) / length(breitbartData$topic)


# fox: top topic is #2
foxData <- joinedDataSet %>% filter(outlet == "foxnews")
foxCount <- as.data.frame(table(foxData$topic))
foxProp <- max(foxCount$Freq) / length(foxData$topic)



# nyt: top topic is #4
nytData <- joinedDataSet %>% filter(outlet == "nytimes")
nytCount <- as.data.frame(table(nytData$topic))
nytProp <- max(nytCount$Freq) / length(nytData$topic)
nytCount

# wsj: top topic is #4
wsjData <- joinedDataSet %>% filter(outlet == "wallstreetjournal")
wsjCount <- as.data.frame(table(wsjData$topic))
wsjProp <- max(wsjCount$Freq) / length(wsjData$topic)

# thinkprogress: top topic is #6
thinkprogData <- joinedDataSet %>% filter(outlet == "thinkprogress")
thinkprogCount <- as.data.frame(table(thinkprogData$topic))
thinkprogProp <- max(thinkprogCount$Freq) / length(thinkprogData$topic)


outlet_names <- c("breitbart", "foxnews", "nytimes", "wallstreetjournal", "thinkprogress")
topic_proportions <- c(breitbartProp, foxProp, nytProp, wsjProp, thinkprogProp)
top_topic <- 

overallTopTopic <- cbind(outlet_names, topic_proportions)

## join overall proportions

## topic over time by outlet