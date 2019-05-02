####Midterm Wenjie Mei#####

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Epidemiology of Enteric Pathogens Infection in Madagascar"),
  
  # Sidebar with a slider input for number of bins
  navbarPage("Navbar",
             tabPanel("Histogram",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("x", "Select variable of interest:",
                                       list("Age"='a',"Height"='b',"Weight"='c')
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
                                       list("Sex"='d',"Activities"='e')
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
                          radioButtons("z", "Select Pathogen of interest:",
                                       list("Shigella"='a',"Entamoeba histolytica"='b',"Salmonella"='c',"Campylobacter"='d')
                          )
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("boxplot")
                        )
                      )
             ),
             
             tabPanel("ANOVA",
                      tableOutput("model")
             ),
             
             tabPanel("Map of study population",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("n",
                                      "Zoom Scale:",
                                      value = 10,
                                      min = 1,
                                      max = 15)
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("map")
                        )
                      )
             ),
             
             tabPanel("Map of Pathogen infection",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("j", "Select Pathogen of interest:",
                                       list("Shigella"='a',"Entamoeba histolytica"='b',"Salmonella"='c',"Campylobacter"='d')
                          )
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("gmap")
                        )
                      )
             )
             
             
             
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  mada<-read.csv('final project_dataset.csv')
  mada$sex<-as.factor(mada$sex)
  mada$newtype<-as.factor(mada$newtype)
  mada$Pathogen1<-as.factor(mada$Pathogen1)
  mada$newage<-as.factor(mada$newage)
  library(ggplot2)
  #library(devtools)
  library(remotes)
  library(glue)
  #devtools::install_github("dkahle/ggmap")
  library(ggplot2)
  library(ggmap)
  
  register_google(key = "AIzaSyD93SYcTFZ9Rzhr_G-oXhN7UoH4SBsC7Dk")
  
  
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
    hist(mada[,i],xlab="Variable of Interest",main="Historgram of Variable of Interest")
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
            panel.border = element_blank(),
            legend.title = element_blank())
    p1
  })
  
  output$table<-renderTable({
    sum1<-table(mada$Pathogen1)
    sum2<-table(mada$Pathogen2)
    sum3<-table(mada$Pathogen3)
    sum4<-table(mada$Pathogen4)
    sum<-rbind(sum1,sum2,sum3,sum4)
    rname<-matrix(c("Shigella","Entamoeba histolytica","Salmonella","Campylobacter"),ncol=1)
    sum<-cbind(rname,sum)
    colnames(sum)<-c("Patghoen","Noninfection","Infection")
    sum
    
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
    ggplot(mada,aes(x=mada[,i], y=age, fill=mada[,i]))+geom_boxplot()+xlab("Pathogen of interest")+theme(axis.text.x = element_blank( ),axis.ticks=element_blank(),legend.title = element_blank())+ scale_fill_discrete(labels = c("Noninfection", "Infection"))
  })
  
  output$model<-renderTable({
    logit2<-glm(Pathogen1 ~ newage + sex + newtype, data = mada, family = "binomial"(link="logit"))
    an1<-anova(logit2, test="Chisq")
    ran<-matrix(c("Null","Age groups","Sex","Activities groups"),ncol=1)
    anova<-cbind(ran,an1)
    colnames(anova)[1]<-("Variables")
    anova
  })
  
  output$gmap<-renderPlot({
    if(input$j=='a'){
      i<-4
    }
    if(input$j=='b'){
      i<-5
    }
    if(input$j=='c'){
      i<-6
    }
    if(input$j=='d'){
      i<-7
    }
    map <- get_map(location = c(lon=mean(mada$long),lat=mean(mada$lattitude)), zoom = 10, scale = 2)
    newdata <- mada[ which(mada[i]=='1'), ]
    ggmap(map) +
      geom_point(data = newdata, aes(x = long, y = lattitude, fill = "red", alpha = 0.8), size =2.5, shape = 21)+
      guides(fill=FALSE, alpha=FALSE, size=FALSE)
    
  })
  
  sliderValues <- reactive({
    
    map <- get_map(location = c(lon=mean(mada$long),lat=mean(mada$lattitude)), zoom = input$n, scale = 2)
    ggmap(map) +
      geom_point(data = mada, aes(x = long, y = lattitude, fill = "red", alpha = 0.8), size =2.5, shape = 21) +
      guides(fill=FALSE, alpha=FALSE, size=FALSE)
    
  })
  
  output$map <- renderPlot({
    sliderValues()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

