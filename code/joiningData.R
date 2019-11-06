library(tidyverse)

# adding independent variables to data set
fullDataSet <- read.csv("preppedDataSet.csv")
demographics <- read.csv("demoData.csv")

fullDataSet <- fullDataSet %>% 
  mutate(date = as.Date(date),
         topic = as.factor(topic))

demographics <- demographics %>% 
  mutate(date = as.Date(date),
         childrenHarmed = factor(ifelse(children == "Yes", 1, 0)),
         countyVote = factor(ifelse(countyVote == "Yes", 1, 0)),
         majorityRace = factor(case_when(
           majorityRace == "White" ~ 1,
           majorityRace == "Black" ~ 2,
           majorityRace == "Latino" ~ 3,
           majorityRace == "Asian" ~ 4,
           majorityRace == "American Indian" ~ 5)),
         medianIncome = as.numeric(medianIncome),
         medianIncome = log(medianIncome),
         zip = as.factor(zip)
  ) %>% 
  select(-murderSuicide, -children) %>% 
  rename(injured = injusted)


joinedDataSet <- left_join(fullDataSet, demographics) %>% 
  fill(c(6:15), .direction = c("down"))

write.csv(joinedDataSet, file = "joinedDataSet.csv", row.names = FALSE)
