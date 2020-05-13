library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(),
    
  ),
  #Application Tile
  titlePanel("Estimation of Time Taken for Processes"),
  plotOutput("ConfidenceInterval"),
  box(Title="Confidence Level 95%:", verbatimTextOutput("ConfiInterval"))
  ,
  
  #sidebar with a slider input for number of bins
  sidebarLayout( 
    sidebarPanel(
      sliderInput("bins", "Maximun Time Taken", min=1, max=100, value=45 ),
      selectInput("ProcessName", "Select a Process", c("Written Resolution", "Transfer of shares", "Letter","Board Meeting","Scanning/Faxing/Emailing of documents (per page)"
                                                       ,"Surveys", "Provision signed registers", "Change in Particulars", "Preparation of Draft Management Account",
                                                       "Liaising with auditors", "Courier Fees: up to 0.5 kg", "Signing of documents/agreements by Director/shareholder",
                                                       "Preparing Resolution/Minutes (minor issues)", "Provision unsigned registers", "Filing of APS", 
                                                       "Bank transfer instruction (per IB instruction)"))
      
    ),
    
    
    
    
    #show a plot of the generated distribution
    mainPanel( plotOutput("distPlot") , height = 300, 
               plotOutput("LinearRegression"), 
               #output summary median, mode
               #output summary median, mode
               box(
                 title = "Median for Linear Regression", 
                 status = "warning", 
                 solidHeader = TRUE,
                 width = 6,
                 height = 142,
                 verbatimTextOutput("MedianMode"))
               ,
               
               box(
                 title = "Median for Histogram", 
                 status = "warning", 
                 solidHeader = TRUE,
                 width = 6,
                 height = 142,
                 verbatimTextOutput("MedianLR"))
               
               
               #mainpanel closing brakect             
    )
    #siderbar panel closing bracket
  )
  #siderbarlayout closing braket
)
# Ui closing bracket




#define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot(
    {
      #import dataset
      DS.2019 <- read.csv('DataSet2019.csv')
      DS.Staff <- filter(DS.2019, Staff_Title == 'Administrator' | Staff_Title ==  'Assistant Administrator' | Staff_Title == 'Assistant Corporate Administrator' | 
                           Staff_Title == 'Assistant Trust Administrator' | Staff_Title == 'Corporate Administrator' | Staff_Title == 'Fund Administrator' | 
                           Staff_Title == 'Senior Administrator' | Staff_Title == 'Senior Corporate Administrator' | Staff_Title == 'Senior Fund Administrator' | 
                           Staff_Title == 'Senior Trust Administrator' | Staff_Title == 'Trust Administrator')
      
      # DS.AA <- filter(DS.Staff, Process_Name =='Bank transfer instruction (per IB instruction)' & TimeTaken <= input$bins)
      #DS.AA <- filter(DS.Staff, Process_Name =='Transfer of shares' & TimeTaken <= input$bins)
      DS.AA <- filter(DS.Staff, Process_Name ==input$ProcessName & TimeTaken <= input$bins)
      
      #standardize number
      #DS.AA$Number <-scale(DS.AA$Number)
      #DS.AA$TimeTaken <-scale(DS.AA$TimeTaken)
      
      #print(DS.AA)
      pl <- ggplot(DS.AA, aes(x=TimeTaken)) + geom_histogram(aes(fill=Team_Name, binwidth=1))+ labs(title = "Histogram of Bank Transfer", x = "Time in hr", y = "Number of Process", fill = "Team Name") 
      print(pl)
      
      output$MedianLR <- renderPrint(
        {
          summary(DS.AA)
        }
      )
      
    }, width = 1500
  )
  
  output$LinearRegression <-renderPlot(
    {
      #import dataset
      DS.2019 <- read.csv('DataSet2019.csv')
      DS.Staff <- filter(DS.2019, Staff_Title == 'Administrator' | Staff_Title ==  'Assistant Administrator' | Staff_Title == 'Assistant Corporate Administrator' | 
                           Staff_Title == 'Assistant Trust Administrator' | Staff_Title == 'Corporate Administrator' | 
                           Staff_Title == 'Fund Administrator' | Staff_Title == 'Senior Administrator' | 
                           Staff_Title == 'Senior Corporate Administrator' | Staff_Title == 'Senior Fund Administrator' | 
                           Staff_Title == 'Senior Trust Administrator' | Staff_Title == 'Trust Administrator')
      
      #DS.AA <- filter(DS.Staff,  Process_Name =='Bank transfer instruction (per IB instruction)' & TimeTaken <= input$bins)
      DS.AA <- filter(DS.Staff, Process_Name ==input$ProcessName & TimeTaken <= input$bins)
      #standardize number
      #DS.AA$Number <-scale(DS.AA$Number)
      #DS.AA$TimeTaken <-scale(DS.AA$TimeTaken)
      
      pl1 <- lm(TimeTaken ~ Number + Staff_Title, DS.AA)
      print(summary(pl1))
      #confidence Interval in the slope
      output$ConfiInterval <- renderPrint({
        confint(pl1, level = 0.95)})
      
      
      pl2 <-ggplot(DS.AA, aes(x=TimeTaken, y=Number)) + geom_point(aes(shape=Team_Name, color=Team_Name), size=3) +  stat_smooth(method='lm', se=TRUE) + theme_stata()
      print(pl2)
      
      output$MedianMode <- renderPrint(
        {
          
          (summary(pl1)) 
        }
      )
    },  width = 1500
  )
}

shinyApp(ui=ui, server = server)