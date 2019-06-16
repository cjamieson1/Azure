## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(googlesheets)
library(BatchGetSymbols)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(glue)
library(DT)

ui <- dashboardPage(
    dashboardHeader(title = "Azure"),
    dashboardSidebar(),
    dashboardBody(
        img(src = "Fremantle.png",height = "300px"),
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotOutput("plot1", height = 250)),
            
            box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
            )
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

shinyApp(ui, server)