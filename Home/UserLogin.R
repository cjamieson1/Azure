
Auth <- function(username,pw,button){
  user_base <- readRDS("Users/UsersHash.Rds")
  validUsers <- dplyr::filter(user_base,user==username)
  
  if(length(validUsers[[1]])==0){
    login = F 
  } else if(sodium::password_verify(validUsers$password,pw)) {
    User_Logged <- validUsers
    login = T
    
  } else {
    login = F
  }
  
  return(login)
}

User_handle <- function(username){
  user_base <- readRDS("Users/Users.Rds")
  validUsers <- dplyr::filter(user_base,user==username)

return(validUsers$name[[1]])
  
}

User_pw <- function(username){
  user_base <- readRDS("Users/Users.Rds")
  validUsers <- dplyr::filter(user_base,user==username)

return(validUsers$password[[1]])

}

User_surname<- function(username){
  user_base <- readRDS("Users/Users.Rds")
  validUsers <- dplyr::filter(user_base,user==username)

return(validUsers$surname[[1]])

}

User_type<- function(username){
  user_base <- readRDS("Users/Users.Rds")
  validUsers <- dplyr::filter(user_base,user==username)
  
  return(validUsers$permissions[[1]])
  
}

Update_profile <- function(username,name,surname,password){
  user_base <- readRDS("Users/Users.Rds")
  validUsers <- dplyr::filter(user_base,user==username)

if(username=="cjamieson1"|username=="Admin"){
  permission = "admin"
} else {
  permission = "standard"
}

newrow <- data.frame("user"=username,"password" = password,"permissions"=permission,"name"=name,"surname"=surname)
print(newrow)
validUsers$name <- name 
validUsers$surname <- surname 
validUsers$password <- password

newbase <- rbind(newrow,user_base)
newbase <- newbase[!duplicated(newbase$user),]
saveRDS(newbase,"Users/Users.Rds")
}

newuser_profile <- function(username,name,surname,password){
  user_base <- readRDS("Users/Users.Rds")
  
  if(username=="cjamieson1"|username=="Admin"){
    permission = "admin"
  } else {
    permission = "standard"
  }
  
  newrow <- data.frame("user"=username,"password" = password,"permissions"=permission,"name"=name,"surname"=surname)
  
  newbase <- rbind(newrow,user_base)
  newbase <- newbase[!duplicated(newbase$user),]
  saveRDS(newbase,"Users/Users.Rds")
}