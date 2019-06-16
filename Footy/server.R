
shinyServer(
  server <- function(input, output, session) {
    
    getfixture <- download.file("https://fixturedownload.com/download/afl-2019-AUSEasternStandardTime.csv",destfile = "fixtures.csv")
    
    winningteam <- function(data){
      Winners <- c(NULL)
      scorehome <- data$Home.Score
      scoreaway <- data$Away.Score
      teamhome <- data$Home.Team
      teamaway <- data$Away.Team
      for(i in 1:length(scorehome)){
        
        if(scorehome[i]>scoreaway[i]&!is.na(scorehome[i])){
          Winners <- rbind(Winners,teamhome[i])
        } else if(scorehome[i]<scoreaway[i]&!is.na(scorehome[i])) {
          Winners <- rbind(Winners,teamaway[i])
        } else {
          Winners <- rbind(Winners,"Draw")
        }
      }
      
      names(Winners)<-"Winner"
      return(Winners)
    }
    
    currentround <- function(data){
      nextgame <- data$Round.Number[data$Date>Sys.time()][1]
      round <- paste("Round",nextgame)
      return(round)
    }
    
    latestdata <- read.csv("fixtures.csv",stringsAsFactors = F)
    newtime <- as.POSIXct(latestdata$Date,format = "%d/%m/%Y %H:%M",tz = "Australia/Sydney")
    attributes(newtime)$tzone <- "Australia/Perth"
    latestdata$Date <- newtime
    latestdata <- dplyr::mutate(latestdata,Home.Score=as.numeric(substr(Result,1,3)))
    latestdata <- dplyr::mutate(latestdata,Away.Score=as.numeric(substr(Result,6,8)))
    latestdata <- cbind(latestdata,winningteam(latestdata))
    print(latestdata)
    names(latestdata) <- c("Round.Number","Date","Location","Home.Team","Away.Team","Result","Home.Score","Away.Score","Winner")
    latestdata <<- latestdata
    AFLteams <- sort(c("Carlton","Collingwood","Melbourne","Adelaide Crows","Western Bulldogs","Brisbane Lions","St Kilda","GWS Giants","Fremantle","Richmond","Sydney Swans","Essendon","Port Adelaide","Geelong Cats","West Coast Eagles","North Melbourne","Hawthorn","Gold Coast Suns"))
  
    updateSelectInput(session = session,inputId = "round_select",selected = currentround(latestdata))
    
    
    
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
        Userteam <<- User_team(input$login_user)
        WebLogCache <- readRDS("Data/userlog.Rds")
        NewCache <- rbind(WebLogCache,data.frame(Time=paste(as.character(Sys.time()),as.character(Sys.timezone())),Name=Userhandle,User=UserLogged))
        saveRDS(NewCache,"Data/userlog.Rds")
        sendSweetAlert(
          session = session,
          title = "Login Successful",
          text = paste("Good Luck",Userhandle),
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
          menuItem("Sign Up", tabName = "newuser", icon = icon("plus"))
          
        )
        #
      } else {
        sidebarMenu(
          id = "postlogin",
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),selected = T),
          menuItem("Tipping", tabName = "tipping", icon = icon("rocket"),selected = T),
          menuItem("Team Fixtures", tabName = "fixtures", icon = icon("chart-bar"),selected = T),
          menuItem("Leaderboard", tabName = "leaderboard",icon = icon("brain"),selected = F),
          menuItem("My Profile",tabName = "profile",icon = icon("users"))
        
        )
      }
      
    })
    
    
    updateTabsetPanel(session,inputId = "prelogin",selected = "login" )

    
 
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
        selectInput("change_team","Favourite Team",choices = AFLteams,selected = Userteam),
        fileInput("change_picture","Change Display Picture",multiple = F,accept = c(".jpg",".jpeg",".png"))
        
      )
      
    })
    
    
    
    observeEvent(input$change_account,{
      Update_profile(username = UserLogged,name = input$change_firstname,surname = input$change_surname,password = input$change_pw,team = input$change_team)
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
    
    #### Register new user ####
    observeEvent(input$signup,{
      updateTabsetPanel(session,inputId = "prelogin",selected = "newuser" )
      })
    
      
    output$newuseraccount <- renderUI({
      
      widgetUserBox(
        title = "New User",
        subtitle = "Registration",
        type = 2,
        width = 6,
        src = "Users/default.png",
        color = "warning",
        closable = FALSE,
        br(),
        textInput("newuser_firstname","First Name",value = "",placeholder = "Enter Your Name"),
        textInput("newuser_surname","Surname",value = "",placeholder = "Enter Your Surname"),
        textInput("newuser_username","Username",value = "",placeholder = "Enter a Username"),
        passwordInput("newuser_pw","Password",value = ""),
        selectInput("newuser_team","Favourite Team",choices = AFLteams,selected = NULL),
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
      newuser_profile(username = input$newuser_username,name = input$newuser_firstname,surname = input$newuser_surname,password = input$newuser_pw,team=input$newuser_team)
      newusername = input$newuser_username
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
      updateSelectInput(session,"newuser_team",selected = NULL)
      
      filename <- paste0("Tips/",newusername,".Rds")
      usertips <- data.frame("Round.Number" = latestdata$Round.Number,"Tip"=latestdata$Away.Team)
      
      saveRDS(usertips,filename)
      updateTabsetPanel(session,inputId = "prelogin",selected = "login")
      sendSweetAlert(
        session = session,
        title = "Sign Up Successful",
        text = "Lets kick some goals!",
        type = "success"
      )
    }) 
    
    #### Dashboard ####
    append_date_suffix <- function(number){
      dayy <- number
      suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                        dayy %% 10 == 1 ~ 'st',
                        dayy %% 10 == 2 ~ 'nd',
                        dayy %% 10 == 3 ~'rd',
                        TRUE ~ "th")
      paste0(dayy, suff)
    }
    
    output$dashuserinfo <- renderUI({
      input$change_account
      input$login_button
      if(file.exists(paste0("WWW/Users/",UserLogged,".jpg"))){
        srclocation <- paste0("Users/",UserLogged,".jpg")
      } else {
        srclocation <- paste0("Users/","default.png")
      }
      widgetUserBox(
        title = UserLogged,
        subtitle = "Your Stats",
        width = 12,
        type = 2,
        src = srclocation,
        color = "navy",
        
        column(4,align="center",
               h4("Correct Tips"),
               br(),
               knobInput(
                 inputId = "usercorrectKnob",
                 #skin = "tron",
                 readOnly = TRUE,
                 label = NULL,
                 value = leaders()$Correct[leaders()$User==UserLogged],
                 min = 0,
                 displayPrevious = TRUE,
                 fgColor = "#428BCA",
                 inputColor = "#428BCA"
               )),
        column(4,align="center",
               h4("Favourite Team"),
               br(),
               img(src = paste0(Userteam,".png"),width = "50%")
        ),
        column(4,align="center",
               h4("Percentage Correct"),
               br(),
               knobInput(
                 inputId = "userpercentageKnob",
                 readOnly = TRUE,
                 label = NULL,
                 value = round(leaders()$Percentage[leaders()$User==UserLogged],2)*100,
                 min = 0,
                 max = 100,
                 displayPrevious = FALSE,
                 #lineCap = "round",
                 fgColor = "green",
                 inputColor = "green"
               )),
        
       
        
        footer = fluidRow(infoBox(
          "Ladder Position",append_date_suffix(leaders()$Position[leaders()$User==UserLogged]), icon = icon("list")
          ,color = "navy",fill = FALSE
        ),
        infoBox(
          "Points Behind Leader",paste(leaders()$Correct[1]-leaders()$Correct[leaders()$User==UserLogged],"Points"), icon = icon("chart-line")
          ,color = "purple", fill = FALSE
        ),
        infoBox(
          "Your Best Round",paste("Round",5,"-",7,"Points"), icon = icon("trophy")
          ,color = "green",fill = FALSE
        ))
          
      )
    })
    
    
    
    #### Leaderboard #####
    
    getUserstats <- function(filename){
      data <- readRDS(filename)
      data[]<- lapply(data, as.character)
      currentdata <- latestdata
      winners <- currentdata$Winner
      winners[]<- lapply(winners, as.character)
      user <- substr(strsplit(filename, "\\.")[[1]][1],6,20)
      correct <- sum(data$Tip==winners)
      gamesplayed <- sum(as.POSIXct(currentdata$Date,format = "%d/%m/%Y %H:%M")<Sys.time())
      percentage <- correct/gamesplayed
      return(data.frame("User"=user,"Correct"=correct,"Percentage"=percentage))
    }
    
   leaders <- reactive({
     filelist <- list.files("Tips/")
     leaders <- data.frame("User"=NULL,"Correct"=NULL,"Percentage"=NULL)
     for(i in 1:length(filelist)){
       fileloc <- paste0("Tips/",filelist[i])
       info <- getUserstats(filename = fileloc)
       leaders <- rbind(leaders,info)
     }
     
     leaders <- leaders[order(leaders$Correct,decreasing = T),]
     leaders <- data.frame("Position"=1:length(leaders$User), "User"=leaders$User,"Correct"=leaders$Correct,"Percentage"=leaders$Percentage,stringsAsFactors = F)
     print(leaders)
     leaders
   })
   
   output$leaderstable <- DT::renderDataTable({
     table <- DT::datatable(leaders(),colnames = c("Position","User","Score","% Correct"),rownames = F,options = list(pageLength = 25,columnDefs = list(list(className = 'dt-center', targets = c(0,2,3))))) %>% 
       formatPercentage(c("Percentage"),digits = 2) %>% 
       formatStyle(
         'User',
         target = 'row',
         color = styleEqual(UserLogged,"white"),
         backgroundColor = styleEqual(UserLogged,"#040033")
       ) 
     # %>% 
     #   formatStyle(
     #     c('Percentage'),
     #     color = styleInterval(0.7, 'white'),
     #     backgroundColor = styleInterval(0.7,'green')
     #   ) 
     table
   })
   
  
   
   findUser_team<- function(username){
     user_base <- readRDS("Users/Users.Rds")
     print(user_base)
     user_base[]<- lapply(user_base, as.character)
     validUsers <- dplyr::filter(user_base,user==username)
     print(validUsers)
     return(validUsers$team[[1]])
     
   }
   
   output$currentleaderinfo <- renderUI({
     widgetUserBox(
       title = leaders()$User[1],
       subtitle = "Current Leader",
       width = 12,
       type = 2,
       src = paste0("Users/",leaders()$User[1],".jpg"),
       color = "navy",
       column(4,align="center",
              h4("Correct Tips"),
              br(),
              knobInput(
         inputId = "correctKnob",
         #skin = "tron",
         readOnly = TRUE,
         label = NULL,
         value = leaders()$Correct[1],
         min = 0,
         displayPrevious = TRUE,
         fgColor = "#428BCA",
         inputColor = "#428BCA"
       )),
       column(4,align="center",
              h4("Favourite Team"),
              br(),
              img(src = paste0(findUser_team(leaders()$User[1]),".png"),width = "50%")
       ),
       column(4,align="center",
              h4("Percentage Correct"),
              br(),
              knobInput(
         inputId = "percentageKnob",
         readOnly = TRUE,
         label = NULL,
         value = round(leaders()$Percentage[1],2)*100,
         min = 0,
         max = 100,
         displayPrevious = FALSE,
         #lineCap = "round",
         fgColor = "green",
         inputColor = "green"
       )),
      
       footer = "His stats are good!"
     )
   })
    
    #### Tipping ####
   
   
    roundfixtures <- reactive({
      selectedround <- substr(input$round_select,7,10)
      currentdata <- latestdata
      currentdata <- dplyr::mutate(currentdata,Home.Score=substr(Result,1,3))
      currentdata <- dplyr::mutate(currentdata,Away.Score=substr(Result,6,8))
      rounddata <- dplyr::filter(currentdata,Round.Number==selectedround)
      print(rounddata)
    })
   
   usertippingdata <- reactive({
     selectedround <- substr(input$round_select,7,10)
     filename <- paste0("Tips/",UserLogged,".Rds")
     tipdata <- readRDS(filename)
     rounddata <- dplyr::filter(tipdata,Round.Number==selectedround)
   })
    
    #### Match 1 ####
    
    output$fixtures1 <- renderUI({
      match <- 1
      
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw"&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"), 
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M",tz = "Australia/Sydney")>Sys.time()){
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))
                } else {h5(align = "center",paste("You tipped",usertippingdata()[match,2]))}
              ,
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {
        NULL
      }
      
      
    })
    
    ### Match 2 #### 
    output$fixtures2 <- renderUI({
     match <- 2
     if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"),
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                 
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })
    
    #### Match 3 ####
    
    output$fixtures3 <- renderUI({
      match <- 3
      
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"), 
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                 
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })
    
    #### Match 4 ####
    
    output$fixtures4 <- renderUI({
      match <- 4
      
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"), 
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                 
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })
    
    #### Match 5 ####
    
    output$fixtures5 <- renderUI({
      match <- 5
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"), 
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                 
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })
    
    #### Match 6 ####
    
    output$fixtures6 <- renderUI({
      match <- 6
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"), 
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                 
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })
    
    #### Match 7 ####
    
    output$fixtures7 <- renderUI({
      match <- 7
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"), 
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                 
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })
    
    #### Match 8 ####
    
    output$fixtures8 <- renderUI({
      match <- 8
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"),
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                 
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })

    #### Match 9 ####
    
    output$fixtures9 <- renderUI({
      match <- 9
      if(!is.na(roundfixtures()$Date[match])){
      box(
        solidHeader = if(roundfixtures()$Winner[match]!="Draw"){TRUE} else {FALSE},
        title = paste("Match",match),
        background = NULL,
        width = 12,
        status = if(usertippingdata()[match,2]==roundfixtures()$Winner[match]){"success"} else if(roundfixtures()$Date[match]<Sys.time()&roundfixtures()$Winner[match]!="Draw") {"danger"} else {"primary"},
        footer = fluidRow(
          column(12,h4(align="center",format(roundfixtures()$Date[match],format = "%d %B %H:%M"))),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Home.Score[match]),
              header = img(src=paste0(roundfixtures()$Home.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Home.Team[match]), 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h4(roundfixtures()$Location[match]),
              number_color = "grey",
              header = h3("VS"), 
              text = if(as.POSIXct(roundfixtures()$Date[match],format = "%d/%m/%Y %H:%M")>Sys.time()){                
                column(12,align = "center",selectInput(paste0("match",match),label = "Tip",choices = c(roundfixtures()$Away.Team[match],roundfixtures()$Home.Team[match])))                 
                } else {h5(paste("You tipped",usertippingdata()[match,2]))},
              right_border = FALSE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 4,
            descriptionBlock(
              number = h3(roundfixtures()$Away.Score[match]),
              header = img(src=paste0(roundfixtures()$Away.Team[match],".png"),height="50px"), 
              text = h4(roundfixtures()$Away.Team[match]), 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          )
        )
      )} else {NULL}
      
      
    })
    

# Tipping Submission ------------------------------------------------------

observeEvent(input$usertips_submit,{
  selectedround <- substr(input$round_select,7,10)
  latestdata <- read.csv("fixtures.csv",stringsAsFactors = F)
  latestdata <- dplyr::mutate(latestdata,Home.Score=substr(Result,1,3))
  latestdata <- dplyr::mutate(latestdata,Away.Score=substr(Result,6,8))
  rounddata <- dplyr::filter(latestdata,Round.Number==selectedround)
  
  Match1tip <- if(!is.null(input$match1)){
    input$match1
  } else {rounddata$Away.Team[1]}
  
  Match2tip <- if(!is.null(input$match2)){
    input$match2
  } else {rounddata$Away.Team[2]}
  
  Match3tip <- if(!is.null(input$match3)){
    input$match3
  } else {rounddata$Away.Team[3]}
  
  Match4tip <- if(!is.null(input$match4)){
    input$match4
  } else {rounddata$Away.Team[4]}
  
  Match5tip <- if(!is.null(input$match5)){
    input$match5
  } else {rounddata$Away.Team[5]}
  
  Match6tip <- if(!is.null(input$match6)){
    input$match6
  } else {rounddata$Away.Team[6]}
  
  Match7tip <- if(!is.null(input$match7)){
    input$match7
  } else {rounddata$Away.Team[7]}
  
  Match8tip <- if(!is.null(input$match8)){
    input$match8
  } else {rounddata$Away.Team[8]}
  
  Match9tip <- if(!is.null(input$match9)){
    input$match9
  } else {rounddata$Away.Team[9]}
  
  roundtips <- c(Match1tip,Match2tip,Match3tip,Match4tip,Match5tip,Match6tip,Match7tip,Match8tip,Match9tip)
  
  filename <- paste0("Tips/",UserLogged,".Rds")
  
  if(file.exists(filename)){
    usertips <- readRDS(filename)
  } else {
    usertips <- data.frame("Round.Number" = latestdata$Round.Number,"Tip"=latestdata$Away.Team)
  }
  
  numberoftips <- length(usertips[usertips[,1]==selectedround,2])
  usertips[usertips[,1]==selectedround,2] <- roundtips[numberoftips]
  saveRDS(usertips,filename)
  sendSweetAlert(
    session = session,
    title = "Tips Placed",
    text = "Best of Luck!",
    type = "success"
  )
})
    
    
# Team Fixtures -----------------------------------------------------------

    roundmyteam <- reactive({
      shiny::validate(need(!is.null(input$teamfixture_select),message = ""))
      latestdata <- read.csv("fixtures.csv",stringsAsFactors = F)
      latestdata <- dplyr::mutate(latestdata,Home.Score=substr(Result,1,3))
      latestdata <- dplyr::mutate(latestdata,Away.Score=substr(Result,6,8))
      rounddata <- dplyr::filter(latestdata,Home.Team==input$teamfixture_select|Away.Team==input$teamfixture_select)
    })
    
    output$teamfixture_select <- renderUI({
      selectInput("teamfixture_select","Select Team",choices = AFLteams,selected = Userteam,multiple = F)
    })
    
    ### Match 1 #### 
    output$myteam1 <- renderUI({
      match <- 1
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {
          NULL
        }
      
      
    })
    
    ### Match 2 #### 
    output$myteam2 <- renderUI({
      match <- 2
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 3 ####
    
    output$myteam3 <- renderUI({
      match <- 3
  
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 4 ####
    
    output$myteam4 <- renderUI({
      match <- 4
     
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 5 ####
    
    output$myteam5 <- renderUI({
      match <- 5
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 6 ####
    
    output$myteam6 <- renderUI({
      match <- 6
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 7 ####
    
    output$myteam7 <- renderUI({
      match <- 7
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 8 ####
    
    output$myteam8 <- renderUI({
      match <- 8
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 9 ####
    
    output$myteam9 <- renderUI({
      match <- 9
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 10 ####
    
    output$myteam10 <- renderUI({
      match <- 10
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    output$myteam11 <- renderUI({
      match <- 11
      
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {
          NULL
        }
      
      
    })
    
    ### Match 12 #### 
    output$myteam12 <- renderUI({
      match <- 12
     
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 13 ####
    
    output$myteam13 <- renderUI({
      match <- 13
    
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 14 ####
    
    output$myteam14 <- renderUI({
      match <- 14
    
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 15 ####
    
    output$myteam15 <- renderUI({
      match <- 15
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 16 ####
    
    output$myteam16 <- renderUI({
      match <- 16
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 17 ####
    
    output$myteam17 <- renderUI({
      match <- 17
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 18 ####
    
    output$myteam18 <- renderUI({
      match <- 18
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 19 ####
    
    output$myteam19 <- renderUI({
      match <- 19
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
    })
    
    #### Match 20 ####
    
    output$myteam20 <- renderUI({
      match <- 20
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 21 ####
    
    output$myteam21 <- renderUI({
      match <- 21
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    #### Match 23 ####
    
    output$myteam23 <- renderUI({
      match <- 23
      if(!is.na(roundmyteam()$Date[match])){
        box(
          solidHeader = FALSE,
          title = paste("Match",match),
          background = NULL,
          width = 12,
          status = "warning",
          footer = fluidRow(
            column(12,h4(align="center",roundmyteam()$Date[match])),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Home.Score[match]),
                header = img(src=paste0(roundmyteam()$Home.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Home.Team[match]), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 2,
              descriptionBlock(
                number = h4(roundmyteam()$Location[match]),
                number_color = "grey",
                header = h3("VS"), 
                right_border = FALSE,
                margin_bottom = FALSE
              )
            ),
            column(
              width = 5,
              descriptionBlock(
                number = h3(roundmyteam()$Away.Score[match]),
                header = img(src=paste0(roundmyteam()$Away.Team[match],".png"),height="50px"), 
                text = h4(roundmyteam()$Away.Team[match]), 
                right_border = TRUE,
                margin_bottom = FALSE
              )
            )
          )
        )} else {NULL}
      
      
    })
    
    
  }
)
#shiny::shinyApp(ui.R, server)
