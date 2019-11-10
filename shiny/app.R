library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)

# data prep
data <- read.csv("data/joinedDataSet.csv")
data$date <- as.Date(data$date)
data <- data %>% 
  mutate(topic = case_when(
    topic == 1 ~ "GunControl",
    topic == 2 ~ "Police",
    topic == 3 ~ "HumanInterest",
    topic == 4 ~ "NationalSecurity",
    topic == 5 ~ "PittsburghShooting",
    topic == 6 ~ "Politics"))

ui <- fluidPage(
  
  titlePanel("Media Coverage of Gun Violence in the US"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a news outlet to display their distribution of topics"),
      selectInput("outlet", 
                  label = "Choose a news outlet",
                  choices = c("all",
                              "breitbart", 
                              "foxnews",
                              "thinkprogress", 
                              "nytimes"),
                  selected = "all"),
      helpText("Select a time frame between Oct. 1 and Nov. 30, 2018)"),
      dateRangeInput("dateRange", "Date range:",
                     start = "2018-10-01",
                     end   = "2018-11-30")
      ),
  mainPanel(
      plotOutput(outputId = "topicPlot"),
      dataTableOutput("shootinginfo")
    )
  )
)


server <- function(input, output){
  
  # a plot showing topic by outlet for time frame
  output$topicPlot <- renderPlot({
    
   if(input$outlet == "all"){
     outletData <- data %>% 
       filter(date >= input$dateRange[1] & date <= input$dateRange[2])
   }else{
     outletData <- data %>% 
      filter(outlet == input$outlet) %>% 
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
   }
    
    plt <- ggplot(data = outletData, 
                  aes(x = as.factor(topic))) +
      geom_bar(aes(fill = as.factor(topic)),
               color = "black") +
      geom_text(stat = "count", 
                aes(label = ..count..), 
                vjust = 2) +
      scale_fill_manual(values = c("#8c510a", "#d8b365", "#f6e8c3", 
                                   "#c7eae5", "#5ab4ac", "#01665e")) +
      theme(axis.text.x = element_text(angle = 30)) +
      labs(fill = "Topic",
           x = "Topic",
           y = "Count")

    print(plt)
  })
 
  # a table showing the number of shootings and dead/injured
   output$shootinginfo <- renderDataTable({

   shootingData <- data[ !duplicated(data$zip), ]
   
   shootingData <- shootingData %>%
     filter(date >= input$dateRange[1] & date <= input$dateRange[2]) %>% 
     group_by(date) %>% 
     mutate(totalShootings = length(zip),
            totalDeadInjured = sum(total)) %>% 
     ungroup() 
   
   shootingData <- shootingData[ !duplicated(shootingData$date), ]
   
   shootingTable <- shootingData %>% 
     select(totalShootings, totalDeadInjured) %>% 
     summarize_all(sum) 
   
    datatable(shootingTable, 
              rownames = FALSE,
              options = list(searching = FALSE,
                             paging = FALSE,
                             info = FALSE))
  })
}

shinyApp(ui, server)
