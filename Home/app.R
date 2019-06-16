# Load packages ----
library(shiny)
library(quantmod)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)



# Source helpers ----
source("helpers.R")

source("getFinancials.R")


ui <- dashboardPage(title = "Auto-Portfolio",skin = "black",
  dashboardHeader(title = "Knox & White"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Portfolio", icon = icon("th"), tabName = "widgets",badgeLabel = "new",badgeColor = "navy"),
      menuItem("Financials", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Income Statement", tabName = "subitem1"),
               menuSubItem("Balance Sheet", tabName = "subitem2"),
               menuSubItem("Cash Flow", tabName = "subitem3")
      )
    )
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem("dashboard", 
              fluidPage(
                
                titlePanel("Stock Selector"),
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Select a stock to examine. 
                             Information will be collected from Yahoo finance."),
                    
                    textInput("symb", "Symbol", "BHP.AX"),
                    
                    dateRangeInput("dates", 
                                   "Date range",
                                   start = "2013-01-01", 
                                   end = as.character(Sys.Date())),
                    
                    br(),
                    br()
                    
                    
                    
                    ),
                  
                  mainPanel(
                    plotOutput("plot")
                    )
                )
              )),
      tabItem("widgets", 
              
                column(6,box(width = "100%",
                             title = "",
                             status = "primary",boxProfile(
                  src = "Chris.JPG",
                  title = "Chris Jamieson",
                  subtitle = "Creator",
                  boxProfileItemList(
                    bordered = TRUE,
                    boxProfileItem(
                      title = "Age",
                      description = 22
                    ),
                    boxProfileItem(
                      title = "Occupation",
                      description = "Mechanical Engineer"
                    ),
                    boxProfileItem(
                      title = "Company",
                      description = "Woodside"
                    )
                  )
                ))),
                column(6,box(width = "100%",
                             title = "",
                             status = "warning",boxProfile(
                  src = "Marco.jpg",
                  title = "Marco Vissers",
                  subtitle = "Analyst",
                  boxProfileItemList(
                    bordered = TRUE,
                    boxProfileItem(
                      title = "Age",
                      description = 22
                    ),
                    boxProfileItem(
                      title = "Occupation",
                      description = "Finance Scum"
                    ),
                    boxProfileItem(
                      title = "Company",
                      description = "Debit Swiss"
                    )
                  )
                ))
              )),
      tabItem("subitem1", 
              DT::dataTableOutput("stocktable1")),
      tabItem("subitem2", 
              DT::dataTableOutput("stocktable2")),
      tabItem("subitem3", 
              DT::dataTableOutput("stocktable3"))
    )
  )
)

server <- function(input, output, session) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  stockdata <- reactive({
    getFin(input$symb)
  })
  
  output$plot <- renderPlot({
    
    chartSeries(x = dataInput(), theme = chartTheme("white"), 
                type = "line", TA = NULL)
  })
  
  output$stocktable1 <- DT::renderDataTable({
    table1 <- stockdata()
    table <- as.data.frame(table1$IS)
    table
  })
  
  output$stocktable2 <- DT::renderDataTable({
    table1 <- stockdata()
    table <- as.data.frame(table1$BS)
    table
  })
  
  output$stocktable3 <- DT::renderDataTable({
    table1 <- stockdata()
    table <- as.data.frame(table1$CF)
    table
  })
  
}

shinyApp(ui, server)
