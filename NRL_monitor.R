downloadUrl<-"http://www.aussportsbetting.com/historical_data/nrl.xlsx"
#destFolder<-"C:/Users/User/Downloads/"
#The next one is for the black Thinkpad Laptop
#destFolder<-"C:\Users\oldma\Downloads"
download.file(downloadUrl,paste0(destFolder,"nrl.xlsx"),method="auto",mode = "wb")

#library(RCurl)
library(dplyr)
#library(gdata)
library(xlsx)
library(ggplot2)
library(plotly)
#library(smooth)
library(forecast)

#See some help here if I want to properly import the kick off time field:
#https://rpubs.com/kfrost14/SubsetSort


colClasses<-c("Date","numeric","character","character","numeric",
              "numeric","character","character",rep("numeric",40),"character")
nrl<-read.xlsx(paste0(destFolder,"nrl.xlsx"),1,startRow = 2,endRow = 650,
               colClasses = colClasses)

#Standardise names
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

currentYear<-2018
lastYear<-currentYear-1
yearBeforeLast<-currentYear-2
lastYearData<-nrl %>% filter(format(Date,"%Y")==lastYear)
currentYearData<-nrl %>% filter(format(Date,"%Y")==currentYear)
yearBeforeLastData<-nrl %>% filter(format(Date,"%Y")==yearBeforeLast)

getTeamData<-function(x){
  homeData<-currentYearData %>% filter(Home.Team == x |
                                         Away.Team == x)
  homeData1<-homeData %>% filter(Home.Team == x) %>%
    mutate(Differential=Home.Score-Away.Score)
  homeData2<-homeData %>% filter(Away.Team == x) %>%
    mutate(Differential=Away.Score-Home.Score)
  homeData<-union(homeData1,homeData2)
  #Arrange by Date, just show some columns
  homeData<-homeData %>% arrange(Date)
  homeData<-homeData %>% select(Date,Home.Team,Home.Score,Away.Team,
                                Away.Score,Differential,Play.Off.Game.,
                                Over.Time.,Notes)
  homeData
}



homeName<-"Sea Eagles"
awayName<-"Raiders"
homeTeam<-getTeamData(homeName)
awayTeam<-getTeamData(awayName)

homeMean<-mean(homeTeam$Differential)
awayMean<-mean(awayTeam$Differential)

homeTeam$DifferentialMean<-homeMean
awayTeam$DifferentialMean<-awayMean
#Local mean home (last 3 games):
numGames<-length(homeTeam$Differential)
gameLast<-homeTeam$Differential[numGames]
game2ndLast<-homeTeam$Differential[numGames-1]
game3rdLast<-homeTeam$Differential[numGames-2]
homeLocalMean<-(gameLast+game2ndLast+game3rdLast)/3
#Local mean away (last 3 games):
numGames<-length(awayTeam$Differential)
gameLast<-awayTeam$Differential[numGames]
game2ndLast<-awayTeam$Differential[numGames-1]
game3rdLast<-awayTeam$Differential[numGames-2]
awayLocalMean<-(gameLast+game2ndLast+game3rdLast)/3
#Add opponent column, which will be good for plotly later
opponent<-NULL
homeAway<-NULL
for(i in 1:nrow(homeTeam)){
  if(homeTeam[i,"Home.Team"] == homeName){
    opponent<-c(opponent,homeTeam[i,"Away.Team"])
    homeAway<-c(homeAway,"home")
  }
  else {
    opponent<-c(opponent,homeTeam[i,"Home.Team"])
    homeAway<-c(homeAway,"away")
  }
}
homeTeam<-homeTeam %>% mutate(Opponent = opponent)
homeTeam<-homeTeam %>% mutate(Home.Away = homeAway)
#Repeat for awayTeam
opponent<-NULL
homeAway<-NULL
for(i in 1:nrow(awayTeam)){
  if(awayTeam[i,"Away.Team"] == awayName){
    opponent<-c(opponent,awayTeam[i,"Home.Team"])
    homeAway<-c(homeAway,"away")
  }
  else {
    opponent<-c(opponent,awayTeam[i,"Away.Team"])
    homeAway<-c(homeAway,"home")
  }
}
awayTeam<-awayTeam %>% mutate(Opponent = opponent)
awayTeam<-awayTeam %>% mutate(Home.Away = homeAway)

combinedTeams<-rbind(homeTeam,awayTeam)
#Label the teams. Paste a '1' for the homeTeam and '2' for away to order them correctly
teamColumn<-c(rep(paste0("1. ",homeName),nrow(homeTeam)),rep(paste0("2. ",awayName),nrow(awayTeam)))
combinedTeams$Team<-teamColumn
#combinedPlot<-ggplot(data=combinedTeams,aes(x=1:nrow(combinedTeams),y=Differential))+
combinedPlot<-ggplot(data=combinedTeams,aes(x=Date,y=Differential))+
    ggtitle(paste0(homeName," (home) vs ",awayName," (away)"))+ylab("Points Differential")+
    geom_point()+
    geom_hline(aes(yintercept = DifferentialMean),data=combinedTeams,linetype = 3)+
    geom_smooth(method="loess",color = "red")+
    facet_grid(Team ~ .)
combinedPlot
ggplotly(combinedPlot)
#See if they played earlier this year:
earlierRounds<-function(x,homeName,awayName){
  #Returns earlier rounds between two teams
  earlierRounds1<-x %>%
    filter(x$Home.Team==homeName & x$Away.Team==awayName)
  earlierRounds2<-x %>%
    filter(Home.Team==awayName & Away.Team==homeName)
  earlierRoundsTemp<-union(earlierRounds1,earlierRounds2) %>%
    arrange(Date)
  earlierRoundsTemp
}
earlierThisYear<-earlierRounds(currentYearData,homeName,awayName)

#Results from last year:
roundsLastYear<-earlierRounds(lastYearData,homeName,awayName)
#The year before:
roundsYearBeforeLast<-earlierRounds(yearBeforeLastData,homeName,awayName)

printResults<-function(x)
{
  printOut<-""
  for(row in 1:nrow(x)){
    tempRow<-x[row,]
    printOut<-paste0(tempRow$Home.Team,"(h) ",tempRow$Home.Score,
                     " vs ",tempRow$Away.Team," ",tempRow$Away.Score)
    #Add notes:
    if(!is.na(tempRow$Notes)){
      printOut<-paste0(printOut," Notes: ",tempRow$Notes)
    }
    cat(printOut)
    cat("\n")
  }
  
}

#Plotly:
p1<-plot_ly(data=homeTeam,
            x = ~Date,
            y = ~Differential,
            text = ~Opponent,
            type = "scatter",
            mode = "markers",
            name = homeName) %>%
  add_lines(y=~DifferentialMean,
            name = "Mean") %>%
  add_lines(y=homeLocalMean,
            name = "Mean (past 3)") %>%
  add_trace(x = ~Date,
            y = ma(homeTeam$Differential,order=3),
            mode="lines",name="SMA(3)")

p2<-plot_ly(data=awayTeam,
            x = ~Date,
            y = ~Differential,
            text = ~Opponent,
            type = "scatter",
            mode = "markers",
            name = awayName) %>%
  add_lines(y=~DifferentialMean,
            name = "Mean") %>%
  add_lines(y=awayLocalMean,
            name = "Mean (past 3)") %>%
  add_trace(x = ~Date,
            y = ma(awayTeam$Differential,order=3),
            mode="lines",name="SMA(3)")
p<-subplot(p1,p2, nrows = 2, shareX = TRUE)
p
#end plotly

#Print:

printResults(earlierThisYear)

printResults(roundsLastYear)

printResults(roundsYearBeforeLast)



#Need to add hover over dots for home or away too (or a colour change, maybe with factor?)
#Need to do multiple regression with these factors: differential of last 5 games (as separate 
#factors), last 3 games against this particular opponent, 'strength of regression to the mean'


#################################
#Experimental from this point on:
#################################


#Notes: try putting points differential through a logb(x,1.2) function
#so that big blow outs don't factor too much. BUT need to account for 
#negatives first by breaking into 'win and loss by how much.'
#Tried this but didn't work so well: 
#Also try exponential smoothing, so that things closer in time are 
#more significant. ses function (setting initial="simple") and
#explore different values of α

#If they've just come off a big game (let's define as playing a top 4 side) and won,
#they are less likely to score a lot of points in the next game.

#Days rest?

#Forward metres gained?

#Total points against other top 8 teams of this year vs. total points
#against other bottom 8 teams of other team.

#Try finding a team they are similar to. E.g. team A beats team C by 14, team B beats team C
#by 16, then A and B are similar. So if a team has a probably with team A, they should also have
#a problem with team B.

#########################################
#Old approach:
chosenTeamData<-chosenTeamData %>% mutate(Differential=Home.Score-Away.Score)
ggplot(data=chosenTeamData,aes(x=c(1:10),y=Differential))+
  geom_line(color="blue")+geom_abline(color="red")

currentYear<-2017
currentYearData<-nrl %>% filter(format(Date,"%Y")==2017)
#Make a function to search for the team with regular expressions. Over time,
#some teams names have changed slightly.
findTeam<-function(teamList,name){
  returned<-sapply(teamList,function(x){
    test<-grep(name,x,ignore.case = TRUE)
    if(length(test)<1)
      return(FALSE)
    else
      return(TRUE)
  })
  returned
}
#Get team, for home and away games
chosenTeamData<-currentYearData %>% filter(findTeam(Home.Team,chosenTeam)|
                                             findTeam(Away.Team,chosenTeam))
#Find the points differential

#Standardise names

teams<-c("Broncos","Bulldogs","Cowboys","Dragons",
         "Eels","Knights","Panthers","Raiders",
         "Rabbitohs","Roosters","Sea Eagles","Sharks",
         "Storm","Titans","Tigers","Warriors")
for(team in teams){
  cat(team,"\n")
  nrlTest$Home.Team<-sapply(nrlTest$Home.Team,function(x)
                  gsub(paste0("^.*",team),team,x,ignore.case = TRUE))
}
nrlTest$Home.Team
#Delete:
testTeams<-c("Canterbury-Bankstown Bulldogs","Canterbury Bulldogs",
             "Sydney Roosters","Rooters")
for(team in teams){
  gsub(team,team,testTeams)
}
testTeams
testDogs<-"Canterbury-Bankstown Bulldogs"
testResult<-sub("^.*bulldogs","Bulldogs",testDogs,ignore.case = TRUE)
testDogs
testResult
nrlTest<-nrl %>% mutate(Home.Team = sub("Bulldogs","Bulldogs",Home.Team,ignore.case = TRUE))

#


##################################

#The following is when I wanted to scrape from Wikipedia.
#Then I found the awesome download from:
#http://www.aussportsbetting.com/data/historical-nrl-results-and-odds-data/
#Revive the following code if that site disappears or no longer
#provides data.

library(xml2)
library(rvest)

teams<-c("Broncos","Bulldogs","Cowboys","Dragons",
         "Eels","Knights","Panthers","Raiders",
         "Rabbitohs","Roosters","Sea Eagles","Sharks",
         "Storm","Titans","Tigers","Warriors")
nrl2017<-read_html("https://en.wikipedia.org/wiki/2017_NRL_season_results")
currentRound<-7

#Round1 (third table in the original document):
testTable<- nrl2017%>% html_nodes("table")
table1<-testTable %>% .[[3]] %>% html_table(fill=TRUE)
table1<-table1[c(-1,-10),]
View(table1)
chosenTeam<-"Bulldogs"
#Find row in table, and Home or Away
homeTeam<-TRUE
teamIndex<-grep(chosenTeam,table1$Home)
if(length(teamIndex) < 1){
  homeTeam<-FALSE
  teamIndex<-grep(chosenTeam,table1$Away)
  if(length(teamIndex) < 1)
    stop("Sorry, chosen team ",chosenTeam, " couldn't be found.");
}
score<-table1[teamIndex,"Score"]
#Split on non-digits (capital D)
scoreSplit<-strsplit(score,"\\D+")
homeScore<-as.numeric(scoreSplit[[1]][1])
awayScore<-as.numeric(scoreSplit[[1]][2])

#End of wiki scrape code

##################################

#Old plot before doing the combined approach:
#plotTeam<-function(x,title="",rounds){
#    ggplot(data=x,aes(x=1:rounds,y=Differential))+
#        ggtitle(title)+xlab("Round")+ylab("Points Differential")+
#        geom_abline(intercept = mean(x$Differential),slope = 0)+
#        scale_x_discrete(limits = c(1:rounds))+
#        geom_point(colour="red",size=2,show.legend = FALSE)+
#        geom_smooth(method=lm,level=0.8,color = "red")
#}

#Old scratchpad:
library(RCurl)
library(dplyr)
nrl2017<-getURL("https://en.wikipedia.org/wiki/2017_NRL_season_results")
nrl2017<-nrl2017 %>% htmlParse()
testTable<- nrl2017%>% html_nodes("table") %>% .[[3]] %>% html_table()
nrl2017 %>% html_nodes("#Round_6") %>% html_text()
