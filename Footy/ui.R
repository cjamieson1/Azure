
ui <- dashboardPage(
  title = "Footy Tipping",
  skin = "blue",
  
  dashboardHeader(
    title = "Just the tips",
    disable = F,
    tags$li(class = "dropdown", style = "padding: 8px;"),
    #shinyauthr::logoutUI("logout")),
    dropdownMenuOutput("messageMenu")
  ),
  
  
  dashboardSidebar(uiOutput("login")),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #f4f4f4;
                                }

                                '))),
    
    # Login Page --------------------------------------------------------------
    
    
    tabItems(
      tabItem("login",
              includeScript("LoginOnEnter.js"),
              fluidPage(
                fluidRow(column(
                  12, align = 'center', titlePanel("Please Log in to continue")
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
                         )),
                
                
                br(),
                br(),
                fluidRow(
                h3(align = "center","Not a user yet?"),
                column(12,align = "center",actionButton("signup","Sign Up Now"))
                )
                
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
        h1(align="center","Welcome Back"),
        br(),
        #img(src="dash.png",width="100%"),
        fluidPage(
        uiOutput("dashuserinfo")
        )
      ),
      
      # Tipping -----------
      tabItem(
        "tipping",
        h1(align="center","Tipping"),
        br(),
        column(12,align="center",selectInput("round_select","Select Round",choices = paste("Round",1:23),selected = "Round 1" ,multiple = F)),
        fluidPage(
        uiOutput("fixtures1"),
        uiOutput("fixtures2"),
        uiOutput("fixtures3"),
        uiOutput("fixtures4"),
        uiOutput("fixtures5"),
        uiOutput("fixtures6"),
        uiOutput("fixtures7"),
        uiOutput("fixtures8"),
        uiOutput("fixtures9"),
        column(12,align = 'center',
               actionButton("usertips_submit","Submit tips for this round")
        )
        )),
      
      tabItem(
        "fixtures",
        h1(align="center","Team Fixtures"),
        br(),
        column(12,align="center",uiOutput("teamfixture_select")),
        fluidPage(
          uiOutput("myteam1"),
          uiOutput("myteam2"),
          uiOutput("myteam3"),
          uiOutput("myteam4"),
          uiOutput("myteam5"),
          uiOutput("myteam6"),
          uiOutput("myteam7"),
          uiOutput("myteam8"),
          uiOutput("myteam9"),
          uiOutput("myteam10"),
          uiOutput("myteam11"),
          uiOutput("myteam12"),
          uiOutput("myteam13"),
          uiOutput("myteam14"),
          uiOutput("myteam15"),
          uiOutput("myteam16"),
          uiOutput("myteam17"),
          uiOutput("myteam18"),
          uiOutput("myteam19"),
          uiOutput("myteam20"),
          uiOutput("myteam21"),
          uiOutput("myteam22"),
          uiOutput("myteam23")
        )),
      
      # Leaderboard ---------
      
      tabItem(
        "leaderboard",
        h1(align="center","Leaderboard"),
        br(),
        uiOutput("currentleaderinfo"),
        br(),
        DT::dataTableOutput("leaderstable")
        ),
      
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
              )))
      
      )
  )
  
)