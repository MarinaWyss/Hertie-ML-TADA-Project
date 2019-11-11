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
    topic == 3 ~ "HumanInterest",
    topic == 4 ~ "SecondAmendment",
    topic == 5 ~ "Politics", 
    topic == 6 ~ "SchoolShootings"))


# topic by ideology

## prep data
veryLiberal <- data %>% 
  filter(ideology < 2)

liberal <- data %>% 
  filter(ideology >= 2 & ideology < 3)

conservative <- data %>% 
  filter(ideology >= 3 & ideology < 4)

veryConservative <- data %>% 
  filter(ideology >= 4)

## plotting
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


# topics over time

## prep data
timeData <- data %>% 
  group_by(date) %>% 
  mutate(propPolice = (sum(topic == "Police")/length(topic)),
         propNationalSec = (sum(topic == "NationalSecurity")/length(topic)),
         propHumanInterest = (sum(topic == "HumanInterest")/length(topic)),
         propSecondAmendment = (sum(topic == "SecondAmendment")/length(topic)),
         propPolitics = (sum(topic == "Politics")/length(topic)),
         propSchoolShootings = (sum(topic == "SchoolShootings")/length(topic))
  ) %>% 
  ungroup()

timeData <- timeData[ !duplicated(timeData$date), ]

timeData <- timeData %>% 
  mutate(date = as.character(date)) %>% 
  select(starts_with("prop"), date) %>% 
  pivot_longer(-date, 
               names_to = "topic",
               values_to = "prop") %>% 
  mutate(date = as.Date(date))

## plotting

timePlot <- ggplot(timeData, aes(x = date, y = prop)) +
  geom_area(aes(fill = as.factor(topic)), color = "black") + 
  scale_fill_manual(labels = c("Human Interest", "National Security", "Police", 
                               "Politics", "School Shootings", "Second Amendment"),
                    values = c("#d8b365", "#f6e8c3", "#DCE6F0", 
                               "#84dee3", "#43bfde", "#0E77ED")) +
  labs(fill = "Topics", 
       x = "Date", 
       y = "Topics",
       title = "Topic Distribution Over Time")
  
timePlot
