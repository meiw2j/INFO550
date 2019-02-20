#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Percent of Patients treated for Drug/Alcohol Abuse vs. only Alcohol Abuse"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("STATE","Choose a State", choices = unique(data$STATE))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(repmis)
  source_data("https://github.com/skhan8/n-ssats/blob/master/nssatspuf_2017.RData?raw=true")
  data<-nssatspuf_2017[,c("STATE","B_PCT","A_PCT")] 
  
  output$plot<-renderPlot(
    
    ggplot(data = data %>% dplyr::filter(STATE==input$STATE),
           aes(A_PCT, B_PCT))+
      geom_point()
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

