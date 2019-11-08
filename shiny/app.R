library(shinydashboard)
library(shiny)
library(tidyverse)

data <- read.csv("data/fullDataSet.csv")

ui <- fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a news outlet to display their distribution of topics"),
      selectInput("outlet", 
                  label = "Choose a news outlet",
                  choices = c("breitbart", 
                              "foxnews",
                              "thinkprogress", 
                              "washingtonpost",
                              "nytimes"),
                  selected = "nytimes")
      ),
  mainPanel(
      plotOutput(outputId = "topicPlot"))
  )
)

server <- function(input, output){
  
  output$topicPlot <- renderPlot({
    
    outletData <- data %>% 
      filter(outlet == input$outlet)
    
    plt <- ggplot(data = outletData, aes(x = topic)) +
      geom_bar(fill = "purple") 
    
    print(plt)
  })
}

shinyApp(ui, server)
