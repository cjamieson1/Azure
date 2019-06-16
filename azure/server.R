#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(googlesheets)
# library(BatchGetSymbols)
# library(plotly)
# library(shinythemes)
# library(shinyWidgets)
# library(glue)
# library(DT)
# library(dplyr)
# library(shinyalert)
# library(googledrive)
# library(stringr)
# library(mailR)
# library(lubridate)
# library(googlesheets)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
})


