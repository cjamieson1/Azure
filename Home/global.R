# Load packages ----
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}

if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require(shinydashboard)) {
  install.packages("shinydashboard")
  library(shinydashboard)
}

if (!require(shinydashboardPlus)) {
  install.packages("shinydashboardPlus")
  library(shinydashboardPlus)
}

if (!require(shinyWidgets)) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}

if (!require(glue)) {
  install.packages("glue")
  library(glue)
}

if (!require(DT)) {
  install.packages("DT")
  library(DT)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(shinyalert)) {
  install.packages("shinyalert")
  library(shinyalert)
}

if (!require(googledrive)) {
  install.packages("googledrive")
  library(googledrive)
}

if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

if (!require(mailR)) {
  install.packages("mailR")
  library(mailR)
}

if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}

if (!require(googlesheets)) {
  install.packages("googlesheets")
  library(googlesheets)
}

if (!require(DT)) {
  install.packages("DT")
  library(DT)
}

if (!require(BatchGetSymbols)) {
  install.packages("BatchGetSymbols")
  library(BatchGetSymbols)
}

if (!require(shinyBS)) {
  install.packages("shinyBS")
  library(shinyBS)
}


if (!require(xlsx)) {
  install.packages("xlsx")
  library(xlsx)
}

if (!require(sodium)) {
  install.packages("sodium")
  library(sodium)
}
# Source helpers ----

source("getFinancials.R")
source("UserLogin.R")

# Load User Base ---- 
user_base <- read.csv("Users/Users.Rds",header = T)

#Load Data Files 

# Create Chatroom ---- 
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to the TradeRoom!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

#write.csv(user_base,"Users.csv",row.names = F)