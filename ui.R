
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(# App title ----
                  
    titlePanel("CHATTALYZER"),
  

    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Upload Your .txt File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv",
                             ".txt")),
        
        # Horizontal line ----
        tags$hr(),
        tags$hr(),
        
        # Input: Select number of rows to display ----
        radioButtons("disp", "Insights",
                     choices = c(Chatcount = "Chatcount",
                                 #CleanedData = "CleanedData",
                                 UserStat = "UserStat",
                                 ActiveUsers = "ActiveUsers",
                                 InactiveUsers = "InactiveUsers",
                                 Active_Days_Post_Count_Order = "Active_Days_Post_Count_Order",
                                 Active_Days_Chronological_Order = "Active_Days_Chronological_Order",
                                 FrequentWords = "FrequentWords",
                                 #Rarewords = "Rarewords",
                                 #Cluster = "Cluster",
                                 Emotion = "Emotion",
                                 Sentiment = "Sentiment"),
                     selected = "Chatcount"),
        #textInput("Date","Enter The Date:","#19/12/17"),
        submitButton("Track!"),
        
        print(h5("Patience! Good Things Takes Time !"))

        # # value is always yyyy-mm-dd, even if the display format is different
        # dateInput("date1", "Day Wise Frequent Words :", value = "2017-01-01", format = "mm/dd/yy"),
        # dateInput("date2", "Day Wise Emotion :", value = "2017-01-01", format = "mm/dd/yy"),
        # dateInput("date3", "Day Wise Sentiment:", value = "2017-01-01", format = "mm/dd/yy")
        
      
        ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Data file ----
        tableOutput("contents"),
        plotOutput("plot")
        #plotOutput("DateText")
        
      )
      
    )
    ))
