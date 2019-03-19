####Midterm Wenjie Mei#####

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Epidemiology of enteric pathogen infection transmissions in Madagascar"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("x", "Select variable of interest:",
                   list("age"='a',"Height"='b',"Weight"='c')
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Histogram", plotOutput("histogram")),
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("Boxplot", plotOutput("boxPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  mada<-read.csv('Midterm dataset2_Wenjie.csv')
  output$histogram<-renderPlot({
    if(input$x=='a'){
      i<-8
    }
    if(input$x=='b'){
      i<-14
    }
    if(input$x=='c'){
      i<-15
    }
    hist(mada[,i])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

