library(caret)
library(h2o)
library(tidyverse)
library(vip)

h2o.init()   

set.seed(123)  

data <- read.csv("preppedDataSet.csv")
ideology <- read.csv("ideology.csv")
data <- merge(data, ideology)

data$topic <- as.factor(data$topic)
data$X <- NULL

# vizualizations

## overall top topic by outlet (before downsampling)
## calculate topic proportion for each outlet
# breitbart: top topic is #2
breitbartData <- data %>% filter(outlet == "breitbart")

plot1Topic <- ggplot(breitbartData, aes(x=topic, fill=topic))+
  geom_bar(stat="count", width=0.7) + scale_fill_hue()

breitbartCount <- as.data.frame(table(breitbartData$topic))
breitbartCount %>% mutate(n = length(breitbartData$topic),
                          topic_proportion = Freq / n
                            )

breitbartProp <- max(breitbartCount$Freq) / length(breitbartData$topic)


# fox: top topic is #3
foxData <- data %>% filter(outlet == "foxnews")

plot2Topic <- ggplot(foxData, aes(x=topic, fill=topic))+
  geom_bar(stat="count", width=0.7) + scale_fill_hue()
plot2Topic

foxCount <- as.data.frame(table(foxData$topic))
foxProp <- max(foxCount$Freq) / length(foxData$topic)



# nyt: top topic is #4
nytData <- data %>% filter(outlet == "nytimes")
nytCount <- as.data.frame(table(nytData$topic))
nytProp <- max(nytCount$Freq) / length(nytData$topic)
nytCount

# wsj: top topic is #4
wsjData <- data %>% filter(outlet == "wallstreetjournal")
wsjCount <- as.data.frame(table(wsjData$topic))
wsjProp <- max(wsjCount$Freq) / length(wsjData$topic)

# thinkprogress: top topic is #6
thinkprogData <- data %>% filter(outlet == "thinkprogress")
thinkprogCount <- as.data.frame(table(thinkprogData$topic))
thinkprogProp <- max(thinkprogCount$Freq) / length(thinkprogData$topic)

# join plots
grid.arrange(plot1, plot2, ncol=2)



outlet_names <- c("breitbart", "foxnews", "nytimes", "wallstreetjournal", "thinkprogress")
topic_proportions <- c(breitbartProp, foxProp, nytProp, wsjProp, thinkprogProp)
top_topic <- 

overallTopTopic <- cbind(outlet_names, topic_proportions)

## join overall proportions

## topic over time by outlet