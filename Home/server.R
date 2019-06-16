shinyServer(
  server <- function(input, output, session) {
    
   download.file("https://www.asx.com.au/asx/research/ASXListedCompanies.csv",destfile = "Data/ASXData.csv")
    ASXTickers <<- read.csv(file="Data/ASXData.csv",skip = 2)
  
 loadingoptionsPG <<- c("Starting nuclear reactor...",
                      "Invading poland...",
                      "feeding the chickens...",
                      "Drilling for gas...",
                      "Entering the black hole...",
                      "Deleting search history...",
                      "Awakening the force...",
                      "downloading more RAM...",
                      "installing virus 1 of 10...",
                      "Retrieving bitcoins...",
                      "Hacking into the mainframe...",
                      "Cracking military encryption...",
                      "Loading Windows98...",
                      "Miss you man :( ",
                      "im so lonely",
                      "Press Alt+F4 to speed up...",
                      "Entering the Matrix..."
                      )

 loadingoptions <<- loadingoptionsPG
                      
  
    # User Login ----------------------------------------------------------
    
    loginbutton <<- F
    Userhandle <<- "User"
    UserLogged <<- "User"
    UserType <<- "Standard"
    Userpw <<- ""
    
    
    observeEvent(input$login_button,{
      
      loginbutton<<- Auth(isolate(input$login_user),input$login_pw)
      if(loginbutton==T){
        
        Userhandle <<- User_handle(input$login_user)
        UserLogged <<- input$login_user
        Userpw <<- User_pw(input$login_user)
        Usersurname <<- User_surname(input$login_user)
        UserType <<- User_type(input$login_user)
        if(UserLogged!="cjamieson1"){
        WebLogCache <- readRDS("Data/userlog.Rds")
        NewCache <- rbind(WebLogCache,data.frame(Time=paste(as.character(Sys.time()),as.character(Sys.timezone())),Name=Userhandle,User=UserLogged))
        saveRDS(NewCache,"Data/userlog.Rds")}
        sendSweetAlert(
          session = session,
          title = "Login Successful",
          text = paste("Welcome Back",Userhandle),
          type = "success"
        )
        updateTabsetPanel(session,inputId = "postlogin",selected = "dashboard" )
        updateTextInput(session,inputId = "change_firstname",value = Userhandle)
        updateTextInput(session,inputId = "change_surname",value = Usersurname)
        updateTextInput(session,inputId = "change_pw",value = Userpw)
        
      } else {
        sendSweetAlert(
          session = session,
          title = "Login Failed",
          text = "Incorrect Username/Password",
          type = "warning"
        )
      }
    })
    
    output$login <- renderUI({
      input$logout
      print(UserType=="admin")
        if(loginbutton==F|is.null(input$login_user)){
         input$login_button
          sidebarMenu(
            id = "prelogin",
            menuItem("Login", tabName = "login", icon = icon("users")),
            menuItem("Sign Up", tabName = "newuser", icon = icon("plus")),
            menuItem("About", icon = icon("question"), tabName = "About")
            
          )
          #
        } else if(UserType=="admin") {
          sidebarMenu(
            id = "postlogin",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),selected = T),
            menuItem("Moving Now", tabName = "daymovers", icon = icon("rocket"),selected = F,badgeLabel = "live",badgeColor = "navy"),
            menuItem("Heat Map", tabName = "heatmap", icon = icon("map"),selected = F),
            menuItem("Statistics", tabName = "stats",icon = icon("brain"),selected = F),
            menuItem("StockTracker", icon = icon("chart-pie"),startExpanded = F,
                     menuSubItem("Biggest Movers", tabName = "movers",icon = icon("rocket")),
                     menuSubItem("Volume Tracker", tabName = "volume",icon = icon("search-plus")),
                     menuSubItem("Short Positions", tabName = "shorts",icon = icon("sort-amount-down"))),
            menuItem("Financials", icon = icon("bar-chart-o"), startExpanded = FALSE,
                     menuSubItem("Income Statement", tabName = "subitem1"),
                     menuSubItem("Balance Sheet", tabName = "subitem2"),
                     menuSubItem("Cash Flow", tabName = "subitem3")
                     
            ),
            menuItem("Daily Logging", icon = icon("save"), tabName = "DailyLog",startExpanded = FALSE,
                     menuSubItem("Roster", tabName = "roster"),
                     menuSubItem("New Submission", tabName = "log"),
                     menuSubItem("History", tabName = "history"),
                     menuSubItem("Invoicing", tabName = "invoice")),
            #menuItem("Forum", tabName = "forum", icon = icon("envelope-open-text")),
            #menuItem("Chatroom",tabName = "chatroom",icon = icon("comments")),
            #menuItem("My Portfolio", icon = icon("th"), tabName = "portfolio"),
            menuItem("My Profile",tabName = "profile",icon = icon("users")),
            menuItem("About", icon = icon("question"), tabName = "About")
            
          )
        } else {
          sidebarMenu(
            id = "postlogin",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),selected = T),
            menuItem("Moving Now", tabName = "daymovers", icon = icon("rocket"),badgeLabel = "live",badgeColor = "navy",selected = F),
            #menuItem("Heat Map", tabName = "heatmap", icon = icon("map"),selected = F),
            menuItem("Statistics", tabName = "stats",icon = icon("brain"),selected = F),
            menuItem("StockTracker", icon = icon("chart-pie"),startExpanded = F,
                     menuSubItem("Biggest Movers", tabName = "movers",icon = icon("rocket")),
                     menuSubItem("Volume Tracker", tabName = "volume",icon = icon("search-plus")),
                     menuSubItem("Short Positions", tabName = "shorts",icon = icon("sort-amount-down"))),
            menuItem("Financials", icon = icon("bar-chart-o"), startExpanded = FALSE,
                     menuSubItem("Income Statement", tabName = "subitem1"),
                     menuSubItem("Balance Sheet", tabName = "subitem2"),
                     menuSubItem("Cash Flow", tabName = "subitem3")
                     
            ),
            menuItem("My Profile",tabName = "profile",icon = icon("users"))
            #menuItem("About", icon = icon("question"), tabName = "About")
            
          )
        }
    
    })
    
   
    updateTabsetPanel(session,inputId = "prelogin",selected = "login" )
    
    # output$login_failed <- renderText("login_failed",
    #   if(Authenticate()==F){
    #     "Login Failed. Invalid Username/Password"
    #   } else {
    #     ""
    #   }
    # )
     
    
    userportfolio <- reactive({
      user <- UserLogged
      path <- paste0("./Users/",user,"/portfolio.csv")
      portfolio <- read.csv(path,header = T)
      return(portfolio)
    })
    
    uservalue <- reactive({
      value <- as.numeric(sum(userportfolio()$Value)*1.1) #sum(getQuote(userportfolio()$Ticker)[1])
      initvalue <- as.numeric(sum(userportfolio()$Value))
      profit <- value - initvalue
      percent <- (value/initvalue - 1)*100
      vals <- list(value,initvalue,profit,percent)
      return(vals)
    })
    
    
    
    
    # Dashboard ---------------------------------------------------------------
    
    
    output$welcome <- renderText({
      
      glue("Welcome back, heres an update:")
    })
    
    dataInput <- reactive({
      symb = paste0(substr(input$symb,1,3),".AX")
      na.omit(
      quantmod::getSymbols(symb,
                           from = input$dates[1],
                           to = input$dates[2],
                           src = 'yahoo',
                           auto.assign = F)
      )
    })
    
    
    
    output$plot <- renderPlot({
      
      if(input$charttype=="Line"){
        quantmod::lineChart(x = dataInput(),theme = "white",type = "line")
      }else{
        quantmod::candleChart(x = dataInput(),theme = "white",type = "line")
      }
      
      if(input$charttool=="MACD"){
        quantmod::addMACD()
      } else {
        quantmod::addRSI()
      }
      
    })
    
    
    stockquote <- reactive({
      shiny::validate(need(!is.null(input$symb)&input$symb!=""&input$symb!=".AX",message = "Loading Stocks"))
      
      symb = paste0(substr(input$symb,1,3),".AX")
      quote <- getQuote(symb)
    })
    
    stockquote2 <- reactive({
      shiny::validate(need(!is.null(input$symb)&input$symb!=""&input$symb!=".AX",message = "Loading Stocks"))
      print("stockquote2")
      symb = paste0(substr(input$symb,1,3),".AX")
      data <- na.omit(getSymbols(symb,from = today()-months(1), to = today(),auto.assign = F))
      weekchange <- as.numeric(data[nrow(data),6])-as.numeric(data[nrow(data)-5,6])
      week <- c(weekchange,100*weekchange/data[nrow(data)-5,6])
      monthchange <- as.numeric(data[nrow(data),6])-as.numeric(data[1,6])
      month <- c(monthchange,100*monthchange/data[1,6])
      data.frame(week,month)
    })
    
    output$search <- renderUI({
      fluidPage(
        fluidRow(
          
          #column(8,selectizeInput(inputId = "symb","Search Ticker",c("BHP.AX","WPL.AX"),width = "100%")),
           column(6,box(width = "100%",solidHeader = F,status = "primary",selectizeInput(inputId = "symb","Search Ticker",selected = "BHP - BHP GROUP LIMITED",paste(ASXTickers$ASX.code,"-",ASXTickers$Company.name),multiple = F,width = "100%"))),
           column(6,align="center",box(width = "100%",solidHeader = F,status = "primary",h4("Download Company Wrap"),
                  downloadButton("StockWrap",label = "Company Wrap")))
        )
        
          
        
      )
    })
    
    output$StockWrap <- downloadHandler(
      filename = function(){
        paste0(input$symb,"-Company-Wrap.pdf")
      },
      content = function(file){
        ticker <- substr(input$symb,1,3)
        url <- paste0("http://www2.aspecthuntley.com.au/pdf/cba/advancedprofile/today/",ticker,"_advancedprofile.pdf")
        download.file(url,file,mode = "wb")
         }
      
    )
    # Dashboard UI ------------------------------------------------------------
    
    
    output$dashboard <- renderUI({
      shiny::validate(need(!is.null(input$symb)&input$symb!=""&input$symb!=".AX",message = "Loading Stock Data"))
      # week <- dataInput()
      # print(week)
      #week<- tail(weeklyReturn(week[,6]),1)
      #year <-tail(yearlyReturn(dataInput()[,6]),1)
      ticker <- substr(input$symb,1,3)
      stockdata <- getStockdata(ticker)
      print(stockdata)
      fluidPage(
        
        fluidRow(
          
          box(
            solidHeader = FALSE,
            title = "Summary",
            background = NULL,
            width = 12,
            status = "info",
            column(12,align = 'center',h2(getStockname(ticker,ASXTickers))),
            #getStockname(ticker,ASXTickers),
            column(12,align = 'center',h4("Last Price (AUD)")),
            column(12,align = 'center',h3(paste0("$",round(stockdata$price[1],3)))),
            footer = fluidRow(
              
              column(4,descriptionBlock(
                number = paste0(round(stockdata$month[2],2),"%"),
                number_color = if(stockdata$month[2]>0){"green"} else {"red"},
                number_icon = if(stockdata$month[2]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
                header = formatC(stockdata$month[1], format="f", digits=3, big.mark=","), 
                text = "Last Month", 
                right_border = TRUE,
                margin_bottom = FALSE
              )
              ),
              #shinyWidgets::updateProgressBar(session,id = "popupprogress",value = 30),
              column(4,descriptionBlock(
                number = paste0(round(stockdata$week[2],2),"%"), 
                number_color = if(stockdata$week[2]>0){"green"} else {"red"}, 
                number_icon = if(stockdata$week[2]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
                header = round(stockdata$week[1],3), 
                text = "Last Week", 
                right_border = TRUE,
                margin_bottom = FALSE
                
              )
              ),
              #shinyWidgets::updateProgressBar(session,id = "popupprogress",value = 60),
              column(4,descriptionBlock(
                number = paste0(round(stockdata$day[2],2),"%"), 
                number_color = if(stockdata$day[2]>0){"green"} else {"red"}, 
                number_icon = if(stockdata$day[2]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
                header = round(stockdata$day[1],3), 
                text = "Last Day", 
                right_border = TRUE,
                margin_bottom = FALSE
              )
              )
              
              
              
            )
          ),
          
          box(width = 12, title = "Stock History",status = "primary",
              dateRangeInput("dates", 
                             "Date range",
                             start = today()-years(1), 
                             end = as.character(Sys.Date())),
              column(6,radioButtons("charttype",label = "Chart Type",choices = c("Candlebar","Line"),inline = T)),
              column(6,align = "right",radioButtons("charttool",label = "Analysis Tool",choices = c("MACD","RSI"),inline = T)),
              plotOutput("plot"),
              br(),
              helpText("Information collected from Yahoo finance.")
          )
        )
      )
      
      
      
    })
    
    
    # Portfolio Tab -----------------------------------------------------------
    
    
    output$folioplot <- renderPlot({
      plot(x = c(1:10),y = c(1:10))
    })
    
    
    # Portfolio UI ------------------------------------------------------------
    
    
    output$portfolio <- renderUI({
      
      fluidPage(
        box(
          solidHeader = FALSE,
          title = "Summary",
          background = NULL,
          width = 12,
          status = "danger",
          footer = fluidRow(
            column(
              width = 6,
              descriptionBlock(
                number = paste0("$",round(as.numeric(uservalue()[3]),2)), 
                number_color = if(uservalue()[3]>0){"green"} else {"red"}, 
                number_icon = if(uservalue()[3]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
                header = uservalue()[1], 
                text = "PORTFOLIO VALUE", 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 6,
              descriptionBlock(
                number = paste0(round(as.numeric(uservalue()[4]),2),"%"), 
                number_color = if(uservalue()[4]>0){"green"} else {"red"}, 
                number_icon = if(uservalue()[4]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
                header = uservalue()[3], 
                text = "TOTAL PROFIT", 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            )
          )
        ),
        column(6, 
               tableOutput("porttable")
               #   box(
               #   solidHeader = FALSE,
               #   title = "Portfolio",
               #   background = NULL,
               #   width = 12,
               #   status = "primary"
               # )
        ),
        column(6,box(solidHeader = FALSE,
                     title = "History",
                     background = NULL,
                     width = 12,
                     status = "primary",
                     plotOutput("folioplot"))
        )
      )
      
    })
    
    output$porttable <- renderTable({
      table <- userportfolio()
    },striped = T)
    

# Stock Tracker -----------------------------------------------------------

### Movers ### 
    
    # movers table 
    
    
  
    
    output$moversTable <- DT::renderDataTable({
      progressSweetAlert(session = session,id = "moversprogress",title = sample(loadingoptions,1),value = 10,status = "info",striped = T)
      title <- googlesheets::gs_title("MonthMovers")
      shinyWidgets::updateProgressBar(session,id = "moversprogress",title = sample(loadingoptions,1),status = "warning",value = 20)
      moverdata <<- googlesheets::gs_read(ss = title,ws = 1)
      moverdata <<- moverdata[moverdata$weekp<5,]
      shinyWidgets::updateProgressBar(session,id = "moversprogress",title = sample(loadingoptions,1),status = "danger",value = 50)
      table <- DT::datatable(moverdata,colnames = c("Ticker","Last Price","This Day","Day %","This Week","Week %","This Month","Month %"),rownames = F,options = list(pageLength = 50,columnDefs = list(list(className = 'dt-center', targets = 1:7)))) %>% 
        formatCurrency(c("lastprice","day","week","month"),"$",digits = 3) %>% 
        formatPercentage(c("dayp","weekp","monthp"),digits = 2) %>% 
        formatStyle(
          c('monthp',"dayp","weekp"),
          color = styleInterval(0, c('black', 'white')),
          backgroundColor = styleInterval(0, c('red', 'green'))
        )
      shinyWidgets::updateProgressBar(session,id = "moversprogress",status = "success",value = 100)
      shinyWidgets::closeSweetAlert(session)
      table
    })

    daydata  <- reactive({
      invalidateLater(60000,session = session)
      progressSweetAlert(session = session,id = "daymoversprogress",title = sample(loadingoptions,1),value = 10,status = "success",striped = T)
      title_day <- googlesheets::gs_title("DayMovers")
      shinyWidgets::updateProgressBar(session,id = "daymoversprogress",title = sample(loadingoptions,1), status = "warning",value = 50)
      daymoverdata <<- googlesheets::gs_read(ss = title_day,ws = 1)
      daymoverdata$Percent <- daymoverdata$Percent/100
      shinyWidgets::updateProgressBar(session,id = "daymoversprogress",title = sample(loadingoptions,1), status = "danger",value = 80)
      table2 <- DT::datatable(daymoverdata,colnames = c('Stock',	"Trade Time",	"Last",	"Change",	"Percent",	"Open",	"High",	"Low",	"Volume"),rownames = F,options = list(pageLength = 50,columnDefs = list(list(className = 'dt-center', targets = 1:7)))) %>% 
        formatCurrency(c("Last","Change","Open","High","Low"),"$",digits = 3) %>% 
        formatPercentage(c("Percent"),digits = 2) %>% 
        formatStyle(
          c("Percent"),
          color = styleInterval(c(-0.0001,0.0001), c('white',"white",'white')),
          backgroundColor = styleInterval(c(-1,-0.5,-0.0001,0,0.5,1), c("maroon",'red',"darkred","black",'darkgreen','green','limegreen'))
        )
      shinyWidgets::updateProgressBar(session,id = "daymoversprogress",status = "success",value = 100)
      shinyWidgets::closeSweetAlert(session)
      table2
    })
    
    output$daymoversTable <- DT::renderDataTable({
      
      table <- daydata()
    })
    
    output$moversdash <- renderUI({})
    
    ### Shorted Companies ### 
    
   output$shorts <- DT::renderDataTable({
     table <- getShorts(7)
     table <- table[,c(1,2,5,4,3,6)]
     DT::datatable(data = table,rownames = F,options = list(pageLength = 25,columnDefs = list(list(className = 'dt-center', targets = 1:4)))) %>% 
       formatPercentage(c("Change"),digits = 2) %>% 
       formatStyle(
         c("Change"),
         color = styleInterval(c(-0.0001,0.0001), c('white',"white",'white')),
         backgroundColor = styleInterval(c(-0.0001,0), c('red',"black",'green'))
       )
          }) 
      
    
    
    # Financial Tab -----------------------------------------------------------
   
   stockdata <- reactive({
     shiny::validate(need(input$symbFin,"",label = "Loading..."))
     symb = paste0(substr(input$symbFin,1,3),".AX")
     getFin(symb)
   })
    
   output$searchFin <- renderUI({
     fluidPage(
       fluidRow(
         
         #column(8,selectizeInput(inputId = "symb","Search Ticker",c("BHP.AX","WPL.AX"),width = "100%")),
         column(6,box(width = "100%",solidHeader = F,status = "primary",selectizeInput(inputId = "symbFin","Search Ticker",selected = "BHP - BHP GROUP LIMITED",paste(ASXTickers$ASX.code,"-",ASXTickers$Company.name),multiple = F,width = "100%"))),
         column(6,align="center",box(width = "100%",solidHeader = F,status = "primary",h4("Download Company Wrap"),
                                     downloadButton("StockWrapFin",label = "Company Wrap")))
       )
       
       
       
     )
   })
   
   output$StockWrapFin <- downloadHandler(
     filename = function(){
       paste0(input$symbFin,"-Company-Wrap.pdf")
     },
     content = function(file){
       ticker <- substr(input$symbFin,1,3)
       url <- paste0("http://www2.aspecthuntley.com.au/pdf/cba/advancedprofile/today/",ticker,"_advancedprofile.pdf")
       download.file(url,file,mode = "wb")
     }
     
   )
   
    # Income Statement ####
    output$stocktable1 <- DT::renderDataTable({
      table1 <- stockdata()
      table <- as.data.frame(table1$IS)
      DT::datatable(table) %>% 
        formatCurrency(names(table),"$")
    })
    
    output$IS <- renderUI({
     
      loadingState(
        DT::dataTableOutput("stocktable1")
      )
    })
    
    # Balance Sheet ####
    
    output$stocktable2 <- DT::renderDataTable({
      table1 <- stockdata()
      table <- as.data.frame(table1$BS)
      DT::datatable(table) %>% 
        formatCurrency(names(table),"$")
    })
    
    output$BS <- renderUI({
     
      loadingState(
        DT::dataTableOutput("stocktable2")
      )
    })
    
    # Cash Flow ####
    
    output$stocktable3 <- DT::renderDataTable({
      table1 <- stockdata()
      table <- as.data.frame(table1$CF)
      DT::datatable(table) %>% 
        formatCurrency(names(table),"$")
    })
    
    output$CF <- renderUI({
      loadingState(
        DT::dataTableOutput("stocktable3")
      )
    })
    


# Forum -------------------------------------------------------------------

    output$forum <- renderUI({
      Comments <- read.csv("Forum.csv",header = T)
  box(
    title = "Discussion forum",
    status = "primary",
    width = NULL,
    
    for (x in 1:nrow(Comments)) {
      
      userPost(
        id = Comments$Id[x],
        src = NULL,
        author = Comments$Name[x],
        description = Comments$Time[x],
        Comments$Post[x]
      )
    },
    userPost(
      id = Comments$Id[1],
      src = "Users/default.png",
      author = Comments$Name[1],
      description = Comments$Time[1],
      Comments$Post[1]
    ),
    userPost(
      id = Comments$Id[2],
      src = "Users/default.png",
      author = Comments$Name[2],
      description = Comments$Time[2],
      Comments$Post[2]
    )
    
      
    
    
  )
})

# Chatroom -----------
# Create a spot for reactive variables specific to this particular session
sessionVars <- reactiveValues(username = Userhandle)

# Track whether or not this session has been initialized. We'll use this to
# assign a username to unininitialized sessions.
init <- FALSE

# When a session is ended, remove the user and note that they left the room. 
session$onSessionEnded(function() {
  isolate({
    vars$users <- Userhandle
    #vars$users[vars$users != sessionVars$username]
    updateTextInput(session,"user",value = Userhandle)
    vars$chat <- c(vars$chat, paste0(linePrefix(),
                                     tags$span(class="user-exit",
                                               sessionVars$username,
                                               "left the room.")))
  })
})

# Observer to handle changes to the username
observe({
  # We want a reactive dependency on this variable, so we'll just list it here.
  input$user
  
  if (!init){
    # Seed initial username
    sessionVars$username <- Userhandle
    isolate({
      vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                        tags$span(class="user-enter",
                                                  sessionVars$username,
                                                  "entered the room.")))
    })
    init <<- TRUE
   } #else{
  #   # A previous username was already given
  #   isolate({
  #     #if (input$user == sessionVars$username || input$user == ""){
  #       # No change. Just return.
  #      # return()
  #     #}
  #     
  #     # Updating username      
  #     # First, remove the old one
  #     vars$users <- vars$users[vars$users != sessionVars$username]
  #     
  #     # Note the change in the chat log
  #     vars$chat <<- c(vars$chat, paste0(linePrefix(),
  #                                       tags$span(class="user-change",
  #                                                 paste0("\"", sessionVars$username, "\""),
  #                                                 " -> ",
  #                                                 paste0("\"", input$user, "\""))))
  #     
  #     # Now update with the new one
  #     sessionVars$username <- Userhandle
  #   })
  # }
  # Add this user to the global list of users
  isolate(vars$users <- Userhandle)
})

# Keep the username updated with whatever sanitized/assigned username we have
observe({
  updateTextInput(session, "user", 
                  value=Userhandle)    
})

# Keep the list of connected users updated
output$userList <- renderUI({
  tagList(tags$ul( lapply(vars$users, function(user){
    return(tags$li(user))
  })))
})

# Listen for input$send changes (i.e. when the button is clicked)
observe({
  if(input$send < 1){
    # The code must be initializing, b/c the button hasn't been clicked yet.
    return()
  }
  isolate({
    # Add the current entry to the chat log.
    vars$chat <<- c(vars$chat, 
                    paste0(linePrefix(),
                           tags$span(class="username",
                                     tags$abbr(title=Sys.time(), sessionVars$username)
                           ),
                           ": ",
                           tagList(input$entry)))
  })
  # Clear out the text entry field.
  updateTextInput(session, "entry", value="")
})

# Dynamically create the UI for the chat window.
output$chat <- renderUI({
  if (length(vars$chat) > 500){
    # Too long, use only the most recent 500 lines
    vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
  }
  # Save the chat object so we can restore it later if needed.
  saveRDS(vars$chat, "chat.Rds")
  
  # Pass the chat log through as HTML
  HTML(vars$chat)
})
    
# Daily Logging -----------------------------------------------------------

output$roster <- DT::renderDataTable({
  sheet <- googlesheets::gs_title("Roster")
  table <- googlesheets::gs_read(sheet,1)
})

output$GoogleCal <- renderUI({
  tags$iframe(style="height:600px; width:100%",src="https://calendar.google.com/calendar/embed?showTitle=0&amp;mode=WEEK&amp;height=600&amp;wkst=1&amp;bgcolor=%23ffffff&amp;src=32asahaiaeemqklocprpnakkmo%40group.calendar.google.com&amp;color=%230F4B38&amp;ctz=Australia%2FPerth")
})


### New Submission ###

output$stockslog <- renderUI({
      selectizeInput(inputId = "stockslog","Trending Stocks for the Day",paste(ASXTickers$ASX.code,"-",ASXTickers$Company.name),multiple = TRUE,width = "100%")
    })

timeofday <- reactive({
  time <- ymd_hms(Sys.time())
  breaks <- hour(hm("00:00", "12:00", "23:59"))
  labels <- c("Morning", "Afternoon")
  cut(x=hour(time), breaks = breaks, labels = labels, include.lowest=TRUE)
})

observeEvent(input$sessiontime,{
  
  #timeofday <- timeofday()
  if(input$sessiontime=="Afternoon"){
    updateNumericInput(session,"hourslog",value = 1)
  } else {
    updateNumericInput(session,"hourslog",value = 1.5)
  }
  
})

 output$emaillist <- renderUI({
   if(input$EmailLog==T){
     selectizeInput(
       "emaillist",
       "Select Recipients",
       choices = c(
         "Chris Jamieson <chrisjamieson@iinet.net.au>",
         "Marco Vissers <marcorobertvissers@outlook.com>",
         "Scott Clyne <scottycly@gmail.com>",
         "Ronald Jamieson <ronjamieson@iinet.net.au>"
       ),
       selected = c(
         "Chris Jamieson <chrisjamieson@iinet.net.au>",
         "Marco Vissers <marcorobertvissers@outlook.com>",
         "Scott Clyne <scottycly@gmail.com>",
         "Ronald Jamieson <ronjamieson@iinet.net.au>"
       ),
       multiple = T
     )
     
   } else {NULL}
 }) 
 
 output$inclWraps <- renderUI({
   if(input$EmailLog==T){
     checkboxInput("inclWraps","Include Company Wraps",value = F)
   } else {NULL}
 }) 
  
 output$selectedWraps <- renderUI({
   
   if(is.null(input$inclWraps)){
     NULL
   } else if(input$inclWraps==T){
     if(length(input$stockslog)>0){
       choices <- input$stockslog
     } else {
       choices <- c(paste(ASXTickers$ASX.code,"-",ASXTickers$Company.name))
     }
     
     tickers <- str_extract_all(input$summarylog, "#\\S+")
     stockwraps <- HashtagStocks(input$summarylog,ASXTickers = ASXTickers)
     selectizeInput("selectedWraps","Select Companies to Attach",choices = choices,selected = stockwraps, multiple = T)
     
     
   } else {NULL}
 })
 
    observeEvent(input$submitlog,{
      
      firststocks <- c("MOQ - MOQ LIMITED","ONT - 1300 SMILES LIMITED","14D - 1414 DEGREES LIMITED","1ST - 1ST GROUP LIMITED","T3D - 333D LIMITED","TGP - 360 CAPITAL GROUP")
      
      if(is.null(input$PersonLog) | input$summarylog==""| is.null(input$stockslog)){
        sendSweetAlert(
          session = session,
          title = "Uh Oh Spaghettios!",
          text = paste("Please fill in all the boxes",Userhandle),
          type = "warning"
        )
      # } else if(nchar(input$summarylog)<15|grepl(pattern = "fucked|fuck|shit|cunt|gay|suck|cock|dick|balls|pussy|faggot|blah|ass|fucking|bla|poo|wanker",x = input$summarylog,ignore.case = T)|sum(stringi::stri_length(strsplit(input$summarylog," |\n")[[1]])>11)>0){
      #   
      #  sendSweetAlert(
      #     session = session,
      #     title = "Oh you done fucked up boy!",
      #     text = paste("Please write a proper summary",Userhandle),
      #     type = "warning"
      #   )
        } else if((sum(input$stockslog %in% firststocks)/length(input$stockslog))>0.5) {
          sendSweetAlert(
            session = session,
            title = "Oi!",
            text = paste("Those selected stocks dont seem right",Userhandle),
            type = "warning"
          )
        
      } else{
        shiny::validate(need(!is.null(input$PersonLog) & !is.null(input$summarylog),message = "Please fill all fields"))
        
        
        progressSweetAlert(session = session,id = "submissionprogress",title = sample(loadingoptions,1),value = 10,status = "danger",striped = T)
        WebLogCache <- readRDS("Data/userlog.Rds")
        if(length(WebLogCache)>0){
        logsheet <- googlesheets::gs_title("Logusers")
        googlesheets::gs_add_row(logsheet,ws = 1,input = WebLogCache)
        blankcache <- data.frame("Time" = NULL, "Name"=NULL,"User"=NULL,row.names = NULL,stringsAsFactors = F)
        saveRDS(blankcache,"Data/userlog.Rds")}
        progressSweetAlert(session = session,id = "submissionprogress",title = sample(loadingoptions,1),value = 20,status = "info",striped = T)
        gsheet1 <- googlesheets::gs_title("HoursGS")
        
        add_data1 <- data.frame("Date" = as.character(input$DateLog),"Day" = strftime(input$DateLog,'%A'),"Person" = input$PersonLog,"Hours"=input$hourslog,"Summary" = input$summarylog,row.names=NULL)
        
        
        
        googlesheets::gs_add_row(gsheet1,ws = 1,input = add_data1)
        shinyWidgets::updateProgressBar(session,id = "submissionprogress",title = sample(loadingoptions,1),value = 30)
        
        gsheet2<- googlesheets::gs_title("StockActivity")
        
        shinyWidgets::updateProgressBar(session,id = "submissionprogress",title = sample(loadingoptions,1),value = 60)
        latestdata <- googlesheets::gs_read(gsheet2,1)
        addstocks <- data.frame("Date"=as.character(input$DateLog),"Ticker" = substr(input$stockslog,1,3),"Stock" =  input$stockslog,"Person" = input$PersonLog)
        addstocks <- write.csv(rbind(latestdata,addstocks),file = "StockActivity.csv",row.names = F)
        googlesheets::gs_upload(file = "StockActivity.csv",overwrite = T)
        shinyWidgets::updateProgressBar(session,id = "submissionprogress",title = sample(loadingoptions,1),value = 90)
        
        if(input$EmailLog==T){
          stocks <- paste0(input$stockslog,"<br>")
          stocks <- paste(stocks,collapse = '')
          summarytext <-str_replace_all(input$summarylog,"\n","<br>") 
          
          stockSheet <- googlesheets::gs_title("StockActivity")
          table <- googlesheets::gs_read(stockSheet,1)
          table$Date <- as.Date(table$Date)
          table <- table %>% filter(Date>today()-days(7))
          plotdata <- as.data.frame(sort(table(table$Ticker),decreasing = T))
          plotdata <- plotdata[1:30,]
          
          p <- plot_ly(data = plotdata,
                       x = plotdata[,1],
                       y = plotdata[,2],
                       #color = "navy",
                        
                       name = "Trending Stocks - This Week",
                       type = "bar"
          ) %>% layout(title = "Trending Stocks - This Week" )
          Sys.setenv("plotly_username"="cjammo")
          Sys.setenv("plotly_api_key"="ASyVslS6CttctXpecWsE")
          plotly_IMAGE(p,width = 600,height = 400,out_file = "WWW/weekly.png")
          
          
          
          if(input$sessiontime=="Afternoon"){
            Messagebody <- paste("Afternoon all, <br><br> Here's is a quick update on the market at closing;<br><br><b> News for the afternoon:</b> <br>", summarytext, "<br><br> <b> The most talked about stocks </b> <br>",'<img src="WWW/weekly.png">',"<br><br>For more info check out: https://jammoitservices.shinyapps.io/KnoxWhite/ <br><br> Thanks, <br> The SIP Team")
            sender <- "SIP Afternoon Commentary <cjamie31@gmail.com>"
            subject <- "Afternoon Wrap-up"
          } else {
            Messagebody <- paste("Morning all, <br><br> Here's is a quick update on the market this morning;<br><br><b> News for the morning:</b> <br>", summarytext, "<br><br> <b> The most talked about stocks </b> <br>",'<img src="WWW/weekly.png">',"<br><br>For more info check out: https://jammoitservices.shinyapps.io/KnoxWhite/ <br><br> Thanks, <br> The SIP Team")
            sender <- "SIP Morning Commentary <cjamie31@gmail.com>"
            subject <- "Morning Wrap-up"
          }
          
          if(length(input$selectedWraps)>0){
           files <- paste0("./Company Wraps/",list.files(path = "./Company Wraps/", pattern = ".pdf"))
           file.remove(files)
           for(x in 1:length(input$selectedWraps)){
             ticker <- substr(input$selectedWraps[x],1,3)
             getWrap(ticker)
           }
           attached <- paste0("./Company Wraps/",list.files(path = "./Company Wraps/", pattern = ".pdf"))
          }else{
            attached <- NULL
          }
          
          mailR::send.mail(from = sender,to = input$emaillist,
                           subject = subject,
                           html = TRUE,
                           body = Messagebody,  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "cjamie31@gmail.com", passwd = "ugxjwuwgtqfnctve", ssl = TRUE),
                           authenticate = TRUE,
                           send = TRUE,attach.files = attached,inline = TRUE)
        }
        
        shinyWidgets::updateProgressBar(session,id = "submissionprogress",value = 99)
        
        
        updateSelectizeInput(session,"PersonLog",selected = "")
        updateTextAreaInput(session,inputId = "summarylog",value = "")
        updateSelectizeInput(session,"stockslog",selected = "")
        
        
        sendSweetAlert(
          session = session,
          title = "Submission Successful",
          text = paste("Thanks",Userhandle),
          type = "success"
        )
        
       
      }
      
  
  
})
    
    observeEvent(input$submittextlog,{
      ChatStocks <- HashtagStocks(input$chattextlog,ASXTickers)
      updateSelectizeInput(session,"stockslog",selected = ChatStocks)
    })


 ### History ### 
    output$loghistory <- renderDataTable({
      input$updatedata
      progressSweetAlert(session = session,id = "historyprogress",title = sample(loadingoptions,1),value = 10,status = "info",striped = T)
      Hourssheet <- googlesheets::gs_title("HoursGS")
      shinyWidgets::updateProgressBar(session,id = "historyprogress",value = 90)
      HoursFile <- googlesheets::gs_download(from = Hourssheet,ws = 1,to = "Data/HoursGS.csv",overwrite = T)
      shinyWidgets::updateProgressBar(session,id = "historyprogress",value = 100)
      shinyWidgets::closeSweetAlert(session)
      table <- read.csv("Data/HoursGS.csv",header = T)
      table[nrow(table):1,]
      
    })
    
    output$hoursworked <- renderPlot({
      
      plotdata <- hoursworked()
      #plotdata$Freq = as.numeric(plotdata$Freq)*1.5
      barplot(plotdata$Total,main = "Hours Worked Per Person",names.arg = row.names(plotdata),col = c("navy","darkgreen","maroon"))
    })
    
    hoursworked <- reactive({
      input$updatedata
      table <- read.csv("Data/HoursGS.csv",header = T)
      table$Date <- as.Date(table$Date)
      Week <- table %>% filter(Date>=floor_date(Sys.Date(), "week"))
      Weekdata <- data.frame(Chris=0,Marco = 0, Scott =0)
      tryCatch({Weekdata$Chris <- sum(Week %>% filter(Person=="Chris") %>% select(Hours))},error=function(e){})
      tryCatch({Weekdata$Marco <- sum(Week %>% filter(Person=="Marco") %>% select(Hours))},error=function(e){})
      tryCatch({Weekdata$Scott <- sum(Week %>% filter(Person=="Scott") %>% select(Hours))},error=function(e){})
      
      Month <- table %>% filter(Date>=floor_date(Sys.Date(), "month"))
      Monthdata <- data.frame(Chris=0,Marco = 0, Scott =0)
      tryCatch({Monthdata$Chris <- sum(Month %>% filter(Person=="Chris") %>% select(Hours))},error=function(e){})
      tryCatch({Monthdata$Marco <- sum(Month %>% filter(Person=="Marco") %>% select(Hours))},error=function(e){})
      tryCatch({Monthdata$Scott <- sum(Month %>% filter(Person=="Scott") %>% select(Hours))},error=function(e){})
     
      
      Total <- table
      Totaldata <- data.frame(Chris=0,Marco = 0, Scott =0)
      Totaldata$Chris <- sum(Total %>% filter(Person=="Chris") %>% select(Hours))
      Totaldata$Marco <- sum(Total %>% filter(Person=="Marco") %>% select(Hours))
      Totaldata$Scott <- sum(Total %>% filter(Person=="Scott") %>% select(Hours))
      plotdata <- as.data.frame(cbind(t(Weekdata),t(Monthdata),t(Totaldata)))
      names(plotdata) <- c("Week","Month","Total")
      plotdata
    })

    
   output$generate <- downloadHandler(
     
     filename = function() {name <- paste0("SIP",input$invnumber,"-",format(input$invoicedates[1],"%b%y"),".xlsx")
     return(name)},
     
     content = function(file){
       
       date1 <- input$invoicedates[1]
       date2 <- input$invoicedates[2]
       progressSweetAlert(session = session,id = "invprogress",title = sample(loadingoptions,1),value = 10,status = "info",striped = T)
       invmonth <- format(date1,"%B")
       hourstitle <- googlesheets::gs_title("HoursGS")
       table <- googlesheets::gs_read(hourstitle,1)#read.csv("HoursGS.csv",header = T)
       table$Date <- as.Date(table$Date)
       Monthtable <- table %>% dplyr::filter(Date>=input$invoicedates[1]) %>% dplyr::filter(Date<input$invoicedates[2])
       Monthdata <- data.frame(Chris=0,Marco = 0, Scott =0)
       tryCatch({Monthdata$Chris <- sum(Monthtable %>% filter(Person=="Chris") %>% select(Hours))},error=function(e){})
       tryCatch({Monthdata$Marco <- sum(Monthtable %>% filter(Person=="Marco") %>% select(Hours))},error=function(e){})
       tryCatch({Monthdata$Scott <- sum(Monthtable %>% filter(Person=="Scott") %>% select(Hours))},error=function(e){})
       
       
       if(input$invoiceperson=="Chris"){
         wb<-loadWorkbook("Invoices/Chris.xlsx")
         sheets <- getSheets(wb)
         sheet <- sheets[["Sheet1"]]
         rows  <- getRows(sheet)
         cells <- getCells(rows)
         shinyWidgets::updateProgressBar(session,id = "invprogress",value = 20)
         setCellValue(cells[["2.2"]], paste0("Invoice #",input$invnumber))
         shinyWidgets::updateProgressBar(session,id = "invprogress",value = 50)
         setCellValue(cells[["11.3"]], Monthdata$Chris)
         setCellValue(cells[["3.2"]], paste("Date:",format.Date(Sys.Date(),"%d %B %Y")))
         shinyWidgets::updateProgressBar(session,id = "invprogress",value = 90)
         setCellValue(cells[["11.2"]], paste("Morning/Afternoon Commentary",format.Date(date1,"%B-%Y")))
         wb$getCreationHelper()$createFormulaEvaluator()$evaluateAll() #This is the trick
         saveWorkbook(wb,"Invoices/christemp.xlsx")
         file.copy("Invoices/christemp.xlsx",file)
         
       #   sheet <- googlesheets::gs_title("InvChris")
       # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 20)
       # print(Monthdata$Chris)
       # filename <- paste0("./Invoices/SIP",input$invnumber,"-",format(date1,"%b%y"),".xlsx")
       # googlesheets::gs_edit_cells(sheet,ws = 1,input = paste0("Invoice #",input$invnumber),anchor = "B2")
       # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 50)
       # googlesheets::gs_edit_cells(sheet,ws = 1,input = paste("Date:",today()),anchor = "B3")
       # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 70)
       # googlesheets::gs_edit_cells(sheet,ws = 1,input = Monthdata$Chris,anchor = "C11")
       # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 90)
       # googlesheets::gs_edit_cells(sheet,ws = 1,input = paste("Morning Commentary",date1,"-",date2),anchor = "B11")
       # googlesheets::gs_download(sheet,ws = 1,to = file,overwrite = T)
       
       } else if(input$invoiceperson=="Scott") {
         
         wb<-loadWorkbook("Invoices/Scott.xlsx")
         sheets <- getSheets(wb)
         sheet <- sheets[["Sheet1"]]
         rows  <- getRows(sheet)
         cells <- getCells(rows)
         shinyWidgets::updateProgressBar(session,id = "invprogress",value = 20)
         setCellValue(cells[["4.5"]], input$invnumber)
         shinyWidgets::updateProgressBar(session,id = "invprogress",value = 50)
         setCellValue(cells[["13.3"]], Monthdata$Scott)
         setCellValue(cells[["3.5"]], format.Date(Sys.Date(),"%d %B %Y"))
         shinyWidgets::updateProgressBar(session,id = "invprogress",value = 90)
         setCellValue(cells[["13.2"]], paste0("Morning Commentary Services Rendered"," (",format.Date(date1,"%B %Y"),")"))
         wb$getCreationHelper()$createFormulaEvaluator()$evaluateAll() #This is the trick
         saveWorkbook(wb,"Invoices/scotttemp.xlsx")
         file.copy("Invoices/scotttemp.xlsx",file)
         
         
         # sheet <- googlesheets::gs_title("InvScott")
         # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 20)
         # 
         # filename <- paste0("./Invoices/SIP",input$invnumber,"-",format(date1,"%b%y"),".xlsx")
         # googlesheets::gs_edit_cells(sheet,ws = 1,input =input$invnumber,anchor = "E4")
         # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 50)
         # googlesheets::gs_edit_cells(sheet,ws = 1,input = format.Date(Sys.Date(),"%B %d, %Y"),anchor = "E3")
         # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 70)
         # googlesheets::gs_edit_cells(sheet,ws = 1,input = Monthdata$Scott,anchor = "C13")
         # shinyWidgets::updateProgressBar(session,id = "invprogress",value = 90)
         # googlesheets::gs_edit_cells(sheet,ws = 1,input = paste0("Morning Commentary Services Rendered"," (",format.Date(date1,"%B %Y"),")"),anchor = "B13")
         # googlesheets::gs_download(sheet,ws = 1,to = file,overwrite = T)
         
       } else {
         library(xlsx)
         wb<-loadWorkbook("Invoices/Marco.xlsx")
         sheets <- getSheets(wb)
         sheet <- sheets[["Sheet1"]]
         rows  <- getRows(sheet)
         cells <- getCells(rows)
         setCellValue(cells[["7.3"]], input$invnumber)
         setCellValue(cells[["22.2"]], Monthdata$Marco)
         setCellValue(cells[["6.3"]], format.Date(Sys.Date(),"%B %d, %Y"))
         
         wb$getCreationHelper()$createFormulaEvaluator()$evaluateAll() #This is the trick
         saveWorkbook(wb,"Invoices/marcotemp.xlsx")
         file.copy("Invoices/marcotemp.xlsx",file)
         
       }
       shinyWidgets::closeSweetAlert(session)
       }
     
   )
   
   output$invnumber <- renderUI({
     invnum <- readRDS("lastinvnum.rds")
     numericInput("invnumber","Invoice Number",value = invnum + 1,width = "50%")
   })
     
   observeEvent(input$invnumber,{
     saveRDS(input$invnumber,"lastinvnum.rds")
   })
     
    output$hourstable <- renderDataTable({
      table <- hoursworked()
      DT::datatable(table, options = list(paging = FALSE,searching = FALSE,pageLength = FALSE))
    })
    
    
    output$moneytable <- renderDataTable({
      data <- hoursworked()
      table <- data*30
      table$Stock <- data$Total*10
      names(table)<-c("Week","Month","Total","Stock")
      table <- DT::datatable(table,options = list(paging = FALSE,searching = FALSE,pageLength = FALSE)) 
      formatCurrency(table = table,c("Week","Month","Total","Stock"),"$")
    })
    
    
    ### Statistics ###
    output$trendingstocks <- renderPlotly({
      
      progressSweetAlert(session = session,id = "statisticsprogress",title = "Loading statistics...",value = 10,status = "info",striped = T)
      Stocks.sheet <- googlesheets::gs_title("StockActivity")
      shinyWidgets::updateProgressBar(session,id = "statisticsprogress",value = 90)
      HoursFile <- googlesheets::gs_download(from = Stocks.sheet,ws = 1,to = "Data/StockActivity.csv",overwrite = T)
      shinyWidgets::updateProgressBar(session,id = "statisticsprogress",value = NULL,total = 100)
      shinyWidgets::closeSweetAlert(session)
      
      table <- read.csv("Data/StockActivity.csv",header = T)
      plotdata <- as.data.frame(sort(table(table$Ticker),decreasing = T))
      plotdata <- plotdata[1:40,]
      #barplot(plotdata,main = "Trending Stocks - Overall",col = c("navy"))
      
      p <- plot_ly(data = plotdata,
        x = plotdata[,1],
        y = plotdata[,2],
        #color = "maroon",
        name = "Trending Stocks - All time",
        type = "bar"
      ) #%>% layout(title = "Trending Stocks - All Time" )
      print(p)
      
    })
    
    plotclick <- reactive({
      event_data("plotly_click")
      toggleModal(session,"StockModal",toggle = "open")
      return(NULL)
    })
    
    output$selection <- renderPrint({
      s <- event_data("plotly_click")
      
      if (length(s) == 0) {
        "Click on a stock to display data"
      } else {
        plotclick()
        cat("You selected: \n\n")
        as.list(s[[3]])
        
      }
    })
    
    
    output$stockpopup <- renderUI({
      ticker <- event_data("plotly_click")[[3]]
      stockdata <- getStockdata(ticker)
      #symb = paste0(substr(ticker,1,3),".AX")
      #plotdata <- getSymbols(symb,from = today()-months(1), to = today(),auto.assign = F)
      box(
        solidHeader = FALSE,
        title = "Summary",
        background = NULL,
        width = 12,
        status = "info",
        column(12,align = 'center',h2(getStockname(ticker,ASXTickers))),
        #getStockname(ticker,ASXTickers),
        column(12,align = 'center',h4("Last Price (AUD)")),
        column(12,align = 'center',h3(paste0("$",stockdata$price[1]))),
        footer = fluidRow(
          
          column(4,descriptionBlock(
            number = paste0(round(stockdata$month[2],2),"%"),
            number_color = if(stockdata$month[2]>0){"green"} else {"red"},
            number_icon = if(stockdata$month[2]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
            header = formatC(stockdata$month[1], format="f", digits=3, big.mark=","), 
            text = "Last Month", 
            right_border = TRUE,
            margin_bottom = FALSE
          )
          ),
          #shinyWidgets::updateProgressBar(session,id = "popupprogress",value = 30),
          column(4,descriptionBlock(
            number = paste0(round(stockdata$week[2],2),"%"), 
            number_color = if(stockdata$week[2]>0){"green"} else {"red"}, 
            number_icon = if(stockdata$week[2]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
            header = round(stockdata$week[1],3), 
            text = "Last Week", 
            right_border = TRUE,
            margin_bottom = FALSE
            
          )
          ),
          #shinyWidgets::updateProgressBar(session,id = "popupprogress",value = 60),
          column(4,descriptionBlock(
            number = paste0(round(stockdata$day[2],2),"%"), 
            number_color = if(stockdata$day[2]>0){"green"} else {"red"}, 
            number_icon = if(stockdata$day[2]>0){"fa fa-caret-up"} else {"fa fa-caret-down"},
            header = round(stockdata$day[1],3), 
            text = "Last Day", 
            right_border = TRUE,
            margin_bottom = FALSE
          )
          )
         
          
          
        )
      )
    })
    
    output$stockchart <- renderPlot({
      ticker <- event_data("plotly_click")[[3]]
      symb = paste0(substr(ticker,1,3),".AX")
      plotdata <- na.omit(getSymbols(symb,from = today()-months(1), to = today(),auto.assign = F))
      quantmod::lineChart(plotdata,theme = "white")
    })
    
    output$trendingstocksweek <- renderPlotly({
      table <- read.csv("Data/StockActivity.csv",header = T)
      table$Date <- as.Date(table$Date)
      table <- table %>% filter(Date>today()-days(7))
      plotdata <- as.data.frame(sort(table(table$Ticker),decreasing = T))
      plotdata <- plotdata[1:30,]
      
      p <- plot_ly(data = plotdata,
                   x = plotdata[,1],
                   y = plotdata[,2],
                   #color = "navy",
                  
                   name = "Trending Stocks - This Week",
                   type = "bar"
      ) #%>% layout(title = "Trending Stocks - This Week" )
      print(p)
    })
    
    output$trendingstocksmonth <- renderPlotly({
      table <- read.csv("Data/StockActivity.csv",header = T)
      table$Date <- as.Date(table$Date)
      table <- table %>% filter(Date>today()-months(1))
      plotdata <- as.data.frame(sort(table(table$Ticker),decreasing = T))
      plotdata <- plotdata[1:40,]
      
      p <- plot_ly(data = plotdata,
                   x = plotdata[,1],
                   y = plotdata[,2],
                   #color = "darkgreen",
                   name = "Trending Stocks - This Month",
                   type = "bar"
      )  #%>% layout(title = "Trending Stocks - This Month")
      print(p)
    })
    
    
    

# Daily Email -------------------------------------------------------------

    # Messagebody <- "Hi Scott \n\n This is just a reminder that you are scheduled for commentary tomorrow morning. \n Start: 6:30am \n Finish: 8:00am \n\n Please remember to log your hours on https://jammoitservices.shinyapps.io/KnoxWhite/ \n\n Thanks \n Mr SIPPY"
    # mailR::send.mail(from = "SIP Morning Commentary <cjamie31@gmail.com>",to = "chrisjamieson@iinet.net.au",subject = "SIP Work Reminder",
    #                  body = Messagebody,  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "cjamie31@gmail.com", passwd = "ugxjwuwgtqfnctve", ssl = TRUE),
    #                  authenticate = TRUE,
    #                  send = TRUE)    
    
    

# Accounts Tabs -----------------------------------------------------------

    output$useraccount <- renderUI({
      input$change_account
      input$login_button
      if(file.exists(paste0("WWW/Users/",UserLogged,".jpg"))){
        srclocation <- paste0("Users/",UserLogged,".jpg")
      } else {
        srclocation <- paste0("Users/","default.png")
      }
      widgetUserBox(
        title = paste(User_handle(UserLogged),Usersurname),
        subtitle = UserType,
        type = NULL,
        width = 6,
        src = srclocation,
        background = TRUE,
        backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
        closable = FALSE,
        br(),
        br(),
        textInput("change_firstname","Edit First Name",value = Userhandle),
        textInput("change_surname","Edit Surname",value = Usersurname),
        passwordInput("change_pw","Edit Password",value = Userpw),
        fileInput("change_picture","Change Display Picture",multiple = F,accept = c(".jpg",".jpeg",".png"))
        
      )
  
    })
    
   
    
observeEvent(input$change_account,{
    Update_profile(username = UserLogged,name = input$change_firstname,surname = input$change_surname,password = input$change_pw)
  Userhandle <<- input$change_firstname
  Usersurname <<- input$change_surname
  Userpw <<- input$change_pw
  newimage <- input$change_picture
    print(newimage)
})    
    
observeEvent(input$logout,{
    loginbutton <<- F
    updateTabsetPanel(session,inputId = "prelogin",selected = "login" )
})

observeEvent(input$change_picture,{
  
}) 

### Register new user ####

output$newuseraccount <- renderUI({
  
  widgetUserBox(
    title = "New User",
    subtitle = "Registration",
    type = 2,
    width = 6,
    src = "default.png",
    color = "primary",
    closable = FALSE,
    br(),
    textInput("newuser_firstname","First Name",value = "",placeholder = "Enter Your Name"),
    textInput("newuser_surname","Surname",value = "",placeholder = "Enter Your Surname"),
    textInput("newuser_username","Username",value = "",placeholder = "Enter a Username"),
    passwordInput("newuser_pw","Password",value = ""),
    fileInput("newuser_picture","Display Picture",multiple = F,accept = c(".jpg",".png"))
    
  )
  
})

observeEvent(input$newuser_submit,{
  users_available <- readRDS("Users/Users.Rds")
  check <- 0
  if(input$newuser_username=="" | input$newuser_firstname==""| input$newuser_surname=="" | input$newuser_pw==""){
    sendSweetAlert(
      session = session,
      title = "Missing info!",
      text = paste("Please fill in all the boxes"),
      type = "warning"
    )
  } else if(input$newuser_username %in% users_available$user){
    sendSweetAlert(
      session = session,
      title = "Error!",
      text = paste("Sorry that username has been taken!"),
      type = "warning"
    )
  } else {
    check <- 1
  }
  shiny::validate(need(input$newuser_username!="" & input$newuser_firstname!=""& input$newuser_surname!="" & input$newuser_pw!="" & check==1,message = "Please fill all fields"))
  newuser_profile(username = input$newuser_username,name = input$newuser_firstname,surname = input$newuser_surname,password = input$newuser_pw)
  print(readRDS("Users/Users.Rds"))
  userpic <- input$newuser_picture 
  print(userpic)
  library(tools)
  file_end <- tools::file_ext(userpic$name)
  userpic$name <- paste0(input$newuser_username,".",file_end)
  newpath <- paste0("WWW/Users/",userpic$name)
  file.copy(userpic$datapath,newpath,overwrite = T,recursive = T)
  updateTextInput(session,"login_user",value = input$newuser_username)
  updateTextInput(session,"newuser_firstname",value = "")
  updateTextInput(session,"newuser_surname",value = "")
  updateTextInput(session,"newuser_username",value = "")
  
  sendSweetAlert(
    session = session,
    title = "Sign Up Successful",
    text = "Welcome to Knox & White",
    type = "success"
  )
}) 

    # Header and Messages -----------------------------------------------------
    
    output$messageMenu <- renderMenu({
      

      
      dropdownMenu(type = "messages",
                   messageItem(
                     from = "Sales Dept",
                     message = "Sales are steady this month."
                   ),
                   messageItem(
                     from = "New User",
                     message = "How do I register?",
                     icon = icon("question"),
                     time = "13:45"
                   ),
                   messageItem(
                     from = "Support",
                     message = "The new server is ready.",
                     icon = icon("life-ring"),
                     time = "2014-12-01"
                   )
      )
      
    })
    
  }
)
#shiny::shinyApp(ui.R, server)
