####Midterm Wenjie Mei#####

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Epidemiology of enteric pathogen infection transmissions in Madagascar"),
  
  # Sidebar with a slider input for number of bins
  navbarPage("Navbar!",
   tabPanel("Histogram",
     sidebarLayout(
      sidebarPanel(
        radioButtons("x", "Select variable of interest:",
                   list("age"='a',"Height"='b',"Weight"='c')
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
     plotOutput("histogram")
      )
    )
  ),
  tabPanel("Piechart",
           sidebarLayout(
             sidebarPanel(
               radioButtons("y", "Select variable of interest:",
                            list("sex"='d',"activities"='e')
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("pieplot")
             )
           )
  ),
  tabPanel("Table",
              tableOutput("table")
             ),
  
  tabPanel("Boxplot",
           sidebarLayout(
             sidebarPanel(
               radioButtons("z", "Select variable of interest:",
                            list("Pathogen1"='a',"Pathogen2"='b',"Pathogen3"='c',"Pathogen4"='d')
               )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("boxplot")
             )
           )
  ),
  
  tabPanel("Regression model",
           tableOutput("model")
  )
          
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  mada<-read.csv('Midterm dataset2_Wenjie.csv')
  mada$sex<-as.factor(mada$sex)
  mada$newtype<-as.factor(mada$newtype)
  mada$Pathogen1<-as.factor(mada$Pathogen1)
  mada$newage<-as.factor(mada$newage)
  library(ggplot2)
  library(tidyverse)
  library(dplyr)
  
  
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
  
  output$pieplot<-renderPlot({
    if(input$y=='d'){
      i<-9
    }
    if(input$y=='e'){
      i<-12
    }
    agg<-count(mada,mada[,i])
    p1<-ggplot(agg) +
      geom_col(aes(x = 1, y = n, fill = unique(mada[,i])), position = "fill") +
      coord_polar(theta = "y")+theme_bw() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    p1
  })
  
  output$table<-renderTable({
    sum1<-table(mada$Pathogen1)
    sum2<-table(mada$Pathogen2)
    sum3<-table(mada$Pathogen3)
    sum4<-table(mada$Pathogen4)
    sum<-rbind(sum1,sum2,sum3,sum4)
    
  })
  
  output$boxplot<-renderPlot({
    if(input$z=='a'){
      i<-4
    }
    if(input$z=='b'){
      i<-5
    }
    if(input$z=='c'){
      i<-6
    }
    if(input$z=='d'){
      i<-7
    }
    mada[,i]<-as.factor(mada[,i])
    ggplot(mada,aes(x=mada[,i], y=age, fill=mada[,i]))+geom_boxplot()
  })
  
  output$model<-renderTable({
    logit2<-glm(Pathogen1 ~ newage + sex + newtype+ Height+ weight, data = mada, family = "binomial"(link="logit"))
    anova(logit2, test="Chisq")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

