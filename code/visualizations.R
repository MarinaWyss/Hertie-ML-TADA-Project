library(tidyverse)
library(gridExtra)
library(lubridate)

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

## all outlets
overallPlot <- ggplot(data = data, 
                      aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  theme(axis.text.x = element_text(angle = 20,
                                   size = 10)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of Topics: All Outlets")

overallPlot

## very liberal
veryLiberal <- data %>% 
  filter(ideology < 2)

veryLiberalPlot <- ggplot(data = veryLiberal, 
                          aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  theme(axis.text.x = element_text(angle = 20,
                                   size = 10)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of Topics: Very Liberal Outlets")

veryLiberalPlot

## liberal
liberal <- data %>% 
  filter(ideology >= 2 & ideology < 3)

liberalPlot <- ggplot(data = liberal, 
                      aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  theme(axis.text.x = element_text(angle = 20,
                                   size = 10)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of Topics: Liberal Outlets")

liberalPlot

## conservative
conservative <- data %>% 
  filter(ideology >= 3 & ideology < 4)

conservativePlot <- ggplot(data = conservative, 
                           aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  theme(axis.text.x = element_text(angle = 20,
                                   size = 10)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of Topics: Conservative Outlets")

conservativePlot

## very conservative
veryConservative <- data %>% 
  filter(ideology >= 4)

veryConservativePlot <- ggplot(data = veryConservative, 
                               aes(x = topic)) +
  geom_bar(aes(fill = topic),
           color = "black") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = 2) +
  scale_fill_manual(values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  theme(axis.text.x = element_text(angle = 20,
                                   size = 10)) +
  labs(fill = "Topic",
       x = "Topic",
       y = "Count", 
       title = "Distribution of Topics: Very Conservative Outlets")

veryConservativePlot

grid.arrange(veryLiberalPlot, liberalPlot,
             conservativePlot, veryConservativePlot)


# topics over time

## all outlets
allOutlets <- data %>%
  mutate(week = week(date)) %>% 
  group_by(week, topic) %>%
  summarise(n = n()) %>%
  mutate(perweek_freq = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(week = as.Date(case_when(week == 22 ~ "2018-05-28",
                                  week == 23 ~ "2018-06-04", 
                                  week == 24 ~ "2018-06-11", 
                                  week == 25 ~ "2018-06-18", 
                                  week == 26 ~ "2018-06-25", 
                                  week == 27 ~ "2018-07-02", 
                                  week == 28 ~ "2018-07-09", 
                                  week == 29 ~ "2018-07-16", 
                                  week == 30 ~ "2018-07-23", 
                                  week == 31 ~ "2018-07-30")))

timePlot <- ggplot(allOutlets,
                   aes(x = week, y = perweek_freq, fill = topic)) + 
  geom_area(alpha = 0.8, color = "black") + 
  scale_fill_manual(labels = c("Human Interest", "National Security", "Police", 
                               "Politics", "School Shootings", "Second Amendment"),
                    values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  labs(fill = "Topics", 
       x = "Week", 
       y = "Topics",
       title = "Topic Distribution Over Time: All Outlets")

timePlot

## very liberal
veryLiberalTime <- veryLiberal %>%
  mutate(week = week(date)) %>% 
  group_by(week, topic) %>%
  summarise(n = n()) %>%
  mutate(perweek_freq = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(week = as.Date(case_when(week == 22 ~ "2018-05-28",
                                  week == 23 ~ "2018-06-04", 
                                  week == 24 ~ "2018-06-11", 
                                  week == 25 ~ "2018-06-18", 
                                  week == 26 ~ "2018-06-25", 
                                  week == 27 ~ "2018-07-02", 
                                  week == 28 ~ "2018-07-09", 
                                  week == 29 ~ "2018-07-16", 
                                  week == 30 ~ "2018-07-23", 
                                  week == 31 ~ "2018-07-30")))

veryLiberalTimePlot <- ggplot(veryLiberalTime, 
                              aes(x = week, y = perweek_freq, fill = topic)) + 
  geom_area(alpha = 0.8, color = "black") + 
  scale_fill_manual(labels = c("Human Interest", "National Security", "Police", 
                               "Politics", "School Shootings", "Second Amendment"),
                    values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  labs(fill = "Topics", 
       x = "Week of the Year", 
       y = "Topics",
       title = "Topic Distribution Over Time: Very Liberal Outlets")

veryLiberalTimePlot

## liberal
liberalTime <- liberal %>%
  mutate(week = week(date)) %>% 
  group_by(week, topic) %>%
  summarise(n = n()) %>%
  mutate(perweek_freq = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(week = as.Date(case_when(week == 22 ~ "2018-05-28",
                                  week == 23 ~ "2018-06-04", 
                                  week == 24 ~ "2018-06-11", 
                                  week == 25 ~ "2018-06-18", 
                                  week == 26 ~ "2018-06-25", 
                                  week == 27 ~ "2018-07-02", 
                                  week == 28 ~ "2018-07-09", 
                                  week == 29 ~ "2018-07-16", 
                                  week == 30 ~ "2018-07-23", 
                                  week == 31 ~ "2018-07-30")))

liberalTimePlot <- ggplot(liberalTime, 
                              aes(x = week, y = perweek_freq, fill = topic)) + 
  geom_area(alpha = 0.8, color = "black") + 
  scale_fill_manual(labels = c("Human Interest", "National Security", "Police", 
                               "Politics", "School Shootings", "Second Amendment"),
                    values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  labs(fill = "Topics", 
       x = "Week of the Year", 
       y = "Topics",
       title = "Topic Distribution Over Time: Liberal Outlets")

liberalTimePlot

## conservative 
conservativeTime <- conservative %>%
  mutate(week = week(date)) %>% 
  group_by(week, topic) %>%
  summarise(n = n()) %>%
  mutate(perweek_freq = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(week = as.Date(case_when(week == 22 ~ "2018-05-28",
                                  week == 23 ~ "2018-06-04", 
                                  week == 24 ~ "2018-06-11", 
                                  week == 25 ~ "2018-06-18", 
                                  week == 26 ~ "2018-06-25", 
                                  week == 27 ~ "2018-07-02", 
                                  week == 28 ~ "2018-07-09", 
                                  week == 29 ~ "2018-07-16", 
                                  week == 30 ~ "2018-07-23", 
                                  week == 31 ~ "2018-07-30")))

conservativeTimePlot <- ggplot(conservativeTime, 
                          aes(x = week, y = perweek_freq, fill = topic)) + 
  geom_area(alpha = 0.8, color = "black") + 
  scale_fill_manual(labels = c("Human Interest", "National Security", "Police", 
                               "Politics", "School Shootings", "Second Amendment"),
                    values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  labs(fill = "Topics", 
       x = "Week of the Year", 
       y = "Topics",
       title = "Topic Distribution Over Time: Conservative Outlets")

conservativeTimePlot

## very conservative 
veryConservativeTimePlot <- veryConservative %>%
  mutate(week = week(date)) %>% 
  group_by(week, topic) %>%
  summarise(n = n()) %>%
  mutate(perweek_freq = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(week = as.Date(case_when(week == 22 ~ "2018-05-28",
                                  week == 23 ~ "2018-06-04", 
                                  week == 24 ~ "2018-06-11", 
                                  week == 25 ~ "2018-06-18", 
                                  week == 26 ~ "2018-06-25", 
                                  week == 27 ~ "2018-07-02", 
                                  week == 28 ~ "2018-07-09", 
                                  week == 29 ~ "2018-07-16", 
                                  week == 30 ~ "2018-07-23", 
                                  week == 31 ~ "2018-07-30")))

veryConservativeTimePlot <- ggplot(conservativeTime, 
                               aes(x = week, y = perweek_freq, fill = topic)) + 
  geom_area(alpha = 0.8, color = "black") + 
  scale_fill_manual(labels = c("Human Interest", "National Security", "Police", 
                               "Politics", "School Shootings", "Second Amendment"),
                    values = c("#762a83", "#af8dc3", "#e7d4e8", 
                               "#d9f0d3", "#7fbf7b", "#1b7837")) +
  labs(fill = "Topics", 
       x = "Week of the Year", 
       y = "Topics",
       title = "Topic Distribution Over Time: Very Conservative Outlets")

veryConservativeTimePlot