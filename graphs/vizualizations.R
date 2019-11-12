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
