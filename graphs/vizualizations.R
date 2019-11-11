library(tidyverse)

set.seed(123)  

data <- read.csv("preppedDataSet.csv")
ideology <- read.csv("ideology.csv")
data <- merge(data, ideology)

data$topic <- as.factor(data$topic)
data$X <- NULL

data <- data %>% 
  mutate(topic = case_when(
    topic == 1 ~ "Police",
    topic == 2 ~ "NationalSecurity",
    topic == 3 ~ "SecondAmendment",
    topic == 4 ~ "HumanInterest",
    topic == 5 ~ "Politics", 
    topic == 6 ~ "SchoolShootings"))


# datasets
veryLiberal <- data %>% 
  filter(ideology < 2)

liberal <- data %>% 
  filter(ideology >= 2 & ideology < 3)

conservative <- data %>% 
  filter(ideology >= 3 & ideology < 4)

veryConservative <- data %>% 
  filter(ideology >= 4)


# topic by ideology
overallPlot <- ggplot(data = data, 
                          aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#d8b365", "#f6e8c3", "#DCE6F0", 
                               "#84dee3", "#43bfde", "#0E77ED")) +
  theme(axis.text.x = element_text(angle = 25,
                                   size = 12)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of topics")

overallPlot


veryLiberalPlot <- ggplot(data = veryLiberal, 
                      aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#d8b365", "#f6e8c3", "#DCE6F0", 
                               "#84dee3", "#43bfde", "#0E77ED")) +
  theme(axis.text.x = element_text(angle = 25,
                                   size = 12)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of topics")

veryLiberalPlot


liberalPlot <- ggplot(data = liberal, 
                          aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#d8b365", "#f6e8c3", "#DCE6F0", 
                               "#84dee3", "#43bfde", "#0E77ED")) +
  theme(axis.text.x = element_text(angle = 25,
                                   size = 12)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of topics")

liberalPlot


conservativePlot <- ggplot(data = conservative, 
                      aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#d8b365", "#f6e8c3", "#DCE6F0", 
                               "#84dee3", "#43bfde", "#0E77ED")) +
  theme(axis.text.x = element_text(angle = 25,
                                   size = 12)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of topics")

conservativePlot


veryConservativePlot <- ggplot(data = veryConservative, 
                           aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#d8b365", "#f6e8c3", "#DCE6F0", 
                               "#84dee3", "#43bfde", "#0E77ED")) +
  theme(axis.text.x = element_text(angle = 25,
                                   size = 12)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of topics")

veryConservativePlot


# topics over time plot
## very liberal
veryLiberalTime <- veryLiberal  %>%
  group_by(date, topic) %>%
  summarise(n = n()) %>%
  mutate(perday_freq = n / sum(n))

veryLiberalTime$date <- as.numeric(veryLiberalTime$date)

ggplot(veryLiberalTime, aes(x=date, y=perday_freq, fill=topic)) + 
  geom_area(alpha=0.6 , size=1, colour="black") +
  
  labs(fill = "Topic", 
       x = "Date", 
       y = "Topic Frequency", 
       title = "Very liberal outlets: distribution of topics over time")

##liberal
liberalTime <- liberal  %>%
  group_by(date, topic) %>%
  summarise(n = n()) %>%
  mutate(perday_freq = n / sum(n))

liberalTime$date <- as.numeric(liberalTime$date)

ggplot(liberalTime, aes(x=date, y=perday_freq, fill=topic)) + 
  geom_area(alpha=0.6 , size=1, colour="black") +
    
  labs(fill = "Topic", 
       x = "Date", 
       y = "Topic Frequency", 
       title = "Liberal outlets: distribution of topics over time")

## conservative 
conservativeTime <- conservative  %>%
  group_by(date, topic) %>%
  summarise(n = n()) %>%
  mutate(perday_freq = n / sum(n))

conservativeTime$date <- as.numeric(conservativeTime$date)
conservativeTime$topic <- factor(conservativeTime$topic , levels=c("Police", "HumanInterest", 
                                                                   "Politics", "NationalSecurity", 
                                                                   "SchoolShootings", "SecondAmendment") )

ggplot(conservativeTime, aes(x=date, y=perday_freq, fill=topic)) + 
  geom_area(alpha=0.6 , size=1, colour="black") +
  
  labs(fill = "Topic", 
       x = "Date", 
       y = "Topic Frequency", 
       title = "Conservative outlets: distribution of topics over time")

## very conservative 
veryConservativeTime <- veryConservative  %>%
  group_by(date, topic) %>%
  summarise(n = n()) %>%
  mutate(perday_freq = n / sum(n))

veryConservativeTime$date <- as.numeric(veryConservativeTime$date)
veryConservativeTime$topic <- factor(veryConservativeTime$topic , levels=c("Police", "HumanInterest", 
                                                                   "Politics", "NationalSecurity", 
                                                                   "SchoolShootings", "SecondAmendment") )

ggplot(veryConservativeTime, aes(x=date, y=perday_freq, fill=topic)) + 
  geom_area(alpha=0.6 , size=1, colour="black") +
  
  labs(fill = "Topic", 
       x = "Date", 
       y = "Topic Frequency", 
       title = "Very conservative outlets: distribution of topics over time")
