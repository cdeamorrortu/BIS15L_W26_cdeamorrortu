library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)


library(shiny)
ui <- fluidPage(
  
  selectInput("y",
              "Select Variable",
              choices = c("height", "age"),
              selected = "height"),
  plotOutput("plot", width = "500px", height = "500px")
  
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    elephants %>% 
      ggplot(aes(x = sex, y = .data[[input$y]], 
                 fill = sex)) +
      geom_boxplot(alpha = 0.5) +
      labs(title = "Distribution of Age and Height by Sex",
           x = "Sex",
           fill = "sex")
    
    
  })
}

shinyApp(ui, server)