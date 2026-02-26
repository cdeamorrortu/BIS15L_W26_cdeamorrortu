library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)


ui <- fluidPage(
  
  theme = shinythemes :: shinytheme("flatly"),
  
  selectInput("y",
              "Select Sleep Variable",
              choices = c("sleep_total", "sleep_rem",
                          "sleep_cycle", "awake"),
              selected = "sleep_total"),
  
  plotOutput("plot", width = "600px", height = "500px")
  
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    msleep %>% 
      filter(vore!="NA") %>% 
      ggplot(aes(x = vore, 
                 y = .data[[input$y]],
                 fill = vore)) +
      geom_boxplot(alpha = 0.5) +
      labs(title = "Sleep Vairables by Vore Type",
           x = "Vore",
           fill = "Vore Type")
  })
  
}

shinyApp(ui, server)