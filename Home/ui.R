
ui <- dashboardPage(
  title = "Knox & White",
  skin = "black",
  
  dashboardHeader(
    title = "Knox & White",
    disable = F,
    tags$li(class = "dropdown", style = "padding: 8px;"),
    #shinyauthr::logoutUI("logout")),
    dropdownMenuOutput("messageMenu")
  ),
 
  
  dashboardSidebar(uiOutput("login")),
  
  dashboardBody(
    # Login Page --------------------------------------------------------------
   
    
    tabItems(
      tabItem("login",
              includeScript("LoginOnEnter.js"),
              fluidPage(
                fluidRow(column(
                  12, align = 'center', titlePanel("Welcome to Knox & White")
                )),
                fluidRow(br(),
                         br(),
                         column(
                           12,
                           offset = 3,
                           align = 'center',
                           box(
                             title = "User Login",
                             status = "primary",
                             collapsible = F,
                             collapsed = F,
                             solidHeader = T,
                             br(),
                             textInput(
                               "login_user",
                               "Enter Username",
                               placeholder = "Username",
                               width = "50%"
                             ),
                             passwordInput(
                               "login_pw",
                               "Enter Password",
                               placeholder = "Password",
                               width = "50%"
                             ),
                             actionButton(
                               "login_button",
                               "Login",
                               icon = icon("users"),
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             )
                             
                           )
                         ))
                
              )),
      

# Register new user -------------------------------------------------------

tabItem("newuser",
        fluidPage(fluidRow(
          column(12, align = 'center',
                 h1("Sign up"),
                 br(),
                 br()),
          column(12, offset = 3,
                 uiOutput("newuseraccount"))
        ),
        fluidRow(column(12,align = 'center',
                        actionButton("newuser_submit","Register")
        )
        ))),
      
      
      # Dashboard ---------------------------------------------------------------
      
      
      
      tabItem(
        "dashboard",
        h1(align="center","Dashboard"),
        br(),
        h4(align='center',"Welcome back heres an update"),
        br(),
        #br(),
        #titlePanel(textOutput("welcome")),
        uiOutput("search"),
        uiOutput("dashboard")
      ),
      
      tabItem(
        "daymovers",
        h1(align="center","Moving Stocks"),
        br(),
        br(),
        dataTableOutput("daymoversTable")
      ),
      
      tabItem(
        "heatmap",
        h1(align="center","Heat Map"),
        br(),
        br(),
        img(src="heatmap.png")
      ),
      
      tabItem(
        "roster",
        h1(align="center","Work Roster"),
        br(),
        br(),
        #uiOutput("GoogleCal"),
        HTML('<iframe src="https://calendar.google.com/calendar/embed?showTitle=0&amp;mode=WEEK&amp;height=600&amp;wkst=1&amp;bgcolor=%23FFFFFF&amp;src=32asahaiaeemqklocprpnakkmo%40group.calendar.google.com&amp;color=%230F4B38&amp;ctz=Australia%2FPerth" style="border-width:0" width="100%" height="600" frameborder="0" scrolling="no"></iframe>'),
        br(),
        h2("Default Schedule"),
        DT::dataTableOutput("roster")
        
                 ),
      
      
      
      # Portfolio ---------------------------------------------------------------
      
      
      tabItem("portfolio",
              uiOutput("portfolio")),
      

      # Stock Tracker -----------------------------------------------------------
      tabItem("movers",
              fluidPage(
                fluidRow(
                  h1(align="center","Biggest Movers"),
                  br(),
                  br(),
                  dataTableOutput("moversTable")
                )
              )
      ),
      
      tabItem("volume",
              fluidPage(
                #tags$head(includeScript("chat.js")),
                fluidRow(
                  h5("Coming soon!")
                  #uiOutput("test")
                )
              )
      ),
      
       tabItem("shorts",
               h1(align="center","Most Shorted Stocks"),
               br(),
               br(),
         fluidPage(
           fluidRow(
             DT::dataTableOutput("shorts")
           )
         )
        
         ),
 

      
      
      # Financials --------------------------------------------------------------
      
      
      tabItem("subitem1",
              h1(align="center","Financials - Income Statement"),
              br(),
              br(),
              uiOutput("searchFin"),
              DT::dataTableOutput("stocktable1")),
      tabItem("subitem2",
              h1(align="center","Financials - Balance Sheet"),
              br(),
              br(),
              DT::dataTableOutput("stocktable2")),
      tabItem("subitem3",
              h1(align="center","Financials - Cash Flow"),
              br(),
              br(),
              DT::dataTableOutput("stocktable3")),
      
      # Forum -------------------------------------------------------------------
      
      
      tabItem(
        "forum",
        h1(align="center","Forum"),
        br(),
        br(),
        box(
          title = "Discussion forum",
          status = "primary",
          width = NULL,
          userPost(
            id = 1,
            src = "Users/sclyne1.jpg",
            author = "Scott Clyne",
            description = "Shared publicly - 7:30 PM today",
            "Looking for some small cap recommendations with a solid growth outlook"
          ),
          
          userPost(
            id = 2,
            src = "Users/mvissers1.jpg",
            author = "Marco Vissers",
            description = "Shared publicly - 5 days ago",
            "I think that #BHP is looking good this quarter!"
          ),
          userPost(
            id = 3,
            src = "Users/cjamieson1.jpg",
            author = "Chris Jamieson",
            description = "Now",
            textInput("newcomment1", "Have Your Say..", placeholder = "Add to the discussion here"),
            actionButton("postcomment1", "Post Comment")
          )
        )
      ),
      
      
      # Chatroom ----------------------------------------------------------------
      
      
      tabItem("chatroom",
              h1(align="center","Chatroom"),
              br(),
              br(),
              bootstrapPage(
                # We'll add some custom CSS styling -- totally optional
                includeCSS("shinychat.css"),
                
                # And custom JavaScript -- just to send a message when a user hits "enter"
                # and automatically scroll the chat window for us. Totally optional.
                includeScript("sendOnEnter.js"),
                
                div(# Setup custom Bootstrap elements here to define a new layout
                  class = "container-fluid",
                  div(
                    class = "row-fluid",
                    # Set the page title
                    tags$head(tags$title("ShinyChat")),
                    
                    # Create the header
                    div(class = "span6", style = "padding: 10px 0px;",
                        h1("Chatroom")),
                    # The main panel
                    div(
                      class = "row-fluid",
                      mainPanel(# Create a spot for a dynamic UI containing the chat contents.
                        uiOutput("chat"),
                        
                        # Create the bottom bar to allow users to chat.
                        fluidRow(
                          div(class = "span10",
                              textInput("entry", "")),
                          div(class = "span2 center",
                              actionButton("send", "Send"))
                        )),
                      # The right sidebar
                      sidebarPanel(
                        # Let the user define his/her own ID
                        textInput("user", "Your User ID:", value =
                                    ""),
                        tags$hr(),
                        h5("Connected Users"),
                        # Create a spot for a dynamic UI containing the list of users.
                        uiOutput("userList")
                        
                      )
                    )
                  ))
              )
      ),
              #,uiOutput("forum")),
              
              
              # About -------------------------------------------------------------------
              
              
              tabItem("About",
                      h1(align="center","About"),
                      br(),
                      br(),
                      fluidPage(
                        fluidRow(column(2),
                                 column(
                                   8, box(
                                     width = "50%",
                                     title = "",
                                     status = "primary",
                                     boxProfile(
                                       src = "Users/cjamieson1.JPG",
                                       title = "Chris Jamieson",
                                       subtitle = "Creator/Developer",
                                       boxProfileItemList(
                                         bordered = TRUE,
                                         boxProfileItem(title = "Age",
                                                        description = 22),
                                         boxProfileItem(title = "Occupation",
                                                        description = "Mechanical Engineer"),
                                         boxProfileItem(title = "Company",
                                                        description = "Woodside")
                                       )
                                     )
                                   )
                                 ),
                                 column(2)),
                        fluidRow(
                          h5(align = "center","This application was initially set up as a means of recording hours done by myself and a few mates whilst doing commentary on the ASX for Stocks in Play.\n\n The commentary requiring up to date market data and quick analysis of the market prompted me to puch my skills to the test and see what i could come up with.\n\n Everything on the site is not perfect, and all the data is obtained from free sources so may not always be 100% correct or validated. \n\n Hope you enjoy the site!")
                        )
                        
                        # fluidRow(column(2),
                        #          column(
                        #            4, box(
                        #              width = "100%",
                        #              title = "",
                        #              status = "warning",
                        #              boxProfile(
                        #                src = "Users/mvissers1.jpg",
                        #                title = "Marco Vissers",
                        #                subtitle = "Actual Piece of Shit",
                        #                boxProfileItemList(
                        #                  bordered = TRUE,
                        #                  boxProfileItem(title = "Age",
                        #                                 description = 21),
                        #                  boxProfileItem(title = "Occupation",
                        #                                 description = "Finance Scum"),
                        #                  boxProfileItem(title = "Company",
                        #                                 description = "Debit Swiss")
                        #                )
                        #              )
                        #            )
                        #          ),
                        #          column(
                        #            4, box(
                        #              width = "100%",
                        #              title = "",
                        #              status = "warning",
                        #              boxProfile(
                        #                src = "Users/sclyne1.jpg",
                        #                title = "Scott Clyne",
                        #                subtitle = "Literal Slime",
                        #                boxProfileItemList(
                        #                  bordered = TRUE,
                        #                  boxProfileItem(title = "Age",
                        #                                 description = 21),
                        #                  boxProfileItem(title = "Occupation",
                        #                                 description = "CEO and Managing Director"),
                        #                  boxProfileItem(title = "Company",
                        #                                 description = "Sclyne Industries")
                        #                )
                        #              )
                        #            )
                        #          ))
                      )),
              
              
              # User Profile ------------------------------------------------------------
              
              
              tabItem("profile",
                      fluidPage(fluidRow(
                        column(12, align = 'center',
                               h1("My Profile"),
                               br(),
                               br()),
                        column(12, offset = 3,
                               uiOutput("useraccount"))
                      ),
                      fluidRow(column(12,align = 'center',
                        actionButton("change_account","Submit Changes"),
                        actionButton("logout","Log Out")
                      )
                      ))),
    
              
              
              # Daily Logging -----------------------------------------------------------
              
              
              
              tabItem("log",
                      
                      fluidPage(fluidRow(
                        h1(align="center","Log Hours"),
                        br(),
                        br(),
                        box(
                          title = "Session information",
                          status = "primary",
                          solidHeader = T,
                          width = "100%",
                          br(),      
                          column(6,
                                 selectInput(
                                   "sessiontime",
                                   "Session",
                                   choices = c("Morning", "Afternoon"),
                                   multiple = F,
                                   width = "100%"
                                 ),
                                 
                                 selectInput(
                                   "PersonLog",
                                   "Person",
                                   choices = c("Chris", "Marco", "Scott"),
                                   multiple = F,
                                   width = "100%"
                                 )
                          ),
                          column(
                            6,
                            dateInput("DateLog", label = "Date", width = "100%"),
                            numericInput(
                              "hourslog",
                              "Hours Worked",
                              width = "100%",
                              value = 1.5,
                              min = 0,
                              max = 8,
                              step = 0.25
                            )
                          ), 
                          br(),
                          column(12,
                                 uiOutput("stockslog"),
                                 
                                 textAreaInput("chattextlog","Paste Chat Text to Analyse",placeholder = "Paste text from morning chat and this will analyse which stocks were talked about for the morning! (must include hashtags for stocks)"),
                                 actionButton("submittextlog","Analyse Text"),
                                 br(),
                                 br(),
                                 textAreaInput(
                                   "summarylog",
                                   "Daily Summary",
                                   placeholder = "Brief summary of the days events \n What announcements effected the market? \n What stocks were destroyed the previous day?",
                                   height = "200px"
                                 ),
                                 checkboxInput("EmailLog", "Send as Email", value = F),
                                 uiOutput("emaillist"),
                                 uiOutput("inclWraps"),
                                 uiOutput("selectedWraps"),
                                 actionButton("submitlog", "Submit", icon = icon("upload")))
                        )
                        
                      ))),
              
              tabItem(
                "history",
                h1(align="center","Log History"),
                br(),
                br(),
                
                box(
                  title = "Hours Logged Data",
                  status = "primary",
                  width = 6,
                  height = "600px",
                  solidHeader = T,
                  h4("Hours Worked"),
                  dataTableOutput("hourstable"),
                  h4("Cash and Stock Accrued"),
                  dataTableOutput("moneytable"),
                  br(),
                  actionButton("updatedata","Update Data")
                ),
                box(
                  title = "Hours Plot",
                  status = "primary",
                  width = 6,
                  height = "600px",
                  solidHeader = T,
                plotOutput("hoursworked",height = "450px")),
                dataTableOutput("loghistory")
                
              ),
      tabItem("stats",
              h1(align="center","Stock Statistics"),
              br(),
              br(),
              helpText("Tip: Click on a stock to view more info about it!"),
              bsModal("StockModal", "Stock Data","plotclick", size = "large",
                      uiOutput("stockpopup")),
              h3(align="center","This Week"),
              plotlyOutput("trendingstocksweek"),
              h3(align="center","This Month"),
              plotlyOutput("trendingstocksmonth"),
              h3(align="center","All Time"),
              plotlyOutput("trendingstocks"),
              verbatimTextOutput("selection")
      ),
      
      tabItem("invoice",
              
              
              
              h1(align="center","Invoicing"),
              br(),
              br(),
              
              box(
                title = "Generate Invoice",
                status = "primary",
                width = 12,
                solidHeader = T,
                dateRangeInput("invoicedates","Select Date Range for Invoice",width = "50%",max = today(),start = floor_date(Sys.Date()-months(1), "month"),end = floor_date(Sys.Date(), "month")),
                selectInput("invoiceperson","Person",width = "50%",choices = c("Chris","Scott","Mr Poo Poo Pants","Sharko","Narco","El Shitpantso","Danny Darco")),
                uiOutput("invnumber"),
                downloadButton("generate","Generate Invoice")
                )
              
              
      )
              
              
              
              
      )
      #,HTML('<div data-iframe-height></div>')
    )
    
  )