#The app was too slow, in that it did all processing every time you open the webpage.
#Cron will be used to run this file each time a new data file is downloaded, instead of every time
#the webpage is loaded.

loadLibraries<-function() {
  library(shiny)
  library(xlsx)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(forecast)
}

#For server:
dataFolder <- "/srv/shiny-server/nrl/data/"

suppressWarnings(loadLibraries())

colClasses<-c("Date","numeric","character","character","numeric",
              "numeric","character","character",rep("numeric",40),"character")
#nrl<-read.xlsx(paste0(dataFolder,"nrl.xlsx"),1,startRow = 2,endRow = 655,
#               colClasses = colClasses)
#Temp for testing before uploading to server (uncomment above two lines later):

dataFolder <- "data/"
nrl<-read.xlsx(paste0(dataFolder,"nrl.xlsx"),1,startRow = 2,endRow = 655,
               colClasses = colClasses)
#end temp

teams<-c("Broncos","Bulldogs","Cowboys","Dragons",
         "Eels","Knights","Panthers","Raiders",
         "Rabbitohs","Roosters","Sea Eagles","Sharks",
         "Storm","Titans","Tigers","Warriors")

for(team in teams){
  nrl$Home.Team<-sapply(nrl$Home.Team,function(x)
    gsub(paste0("^.*",team),team,x,ignore.case = TRUE))
  nrl$Away.Team<-sapply(nrl$Away.Team,function(x)
    gsub(paste0("^.*",team),team,x,ignore.case = TRUE))
}
#End Standardise names

#currentYear<-as.numeric(format(Sys.Date(),"%Y")) #was: 
currentYear<-2018

lastYear<-currentYear-1
yearBeforeLast<-currentYear-2
lastYearData<-nrl %>% filter(format(Date,"%Y")==lastYear)
currentYearData<-nrl %>% filter(format(Date,"%Y")==currentYear)
yearBeforeLastData<-nrl %>% filter(format(Date,"%Y")==yearBeforeLast)

#Save to a file:
saveFile <- paste0(dataFolder,"nrlData.RData")
save(currentYearData,lastYearData,yearBeforeLastData, file = saveFile)

