
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#Function if we need to install packages:
#getLibraries<-function() {
#  list.of.packages <- c("shiny", "xlsx", "dplyr", "ggplot2", "plotly", "forecast")
#  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#  if(length(new.packages)) install.packages(new.packages)
#}

#Remember to make a script using wget to download the Excel file each Wednesday (or whenever it updates)
#Use crontab -e to add it.
##Script uses:
#
# cd /srv/shiny-server/nrl/data
# mv nrl.xlsx nrl.xlsx.old
# wget --tries=10 http://www.aussportsbetting.com/historical_data/nrl.xlsx
# R < /srv/shiny-server/nrl/prepareData.R --no-save
# touch /srv/shiny-server/nrl/restart.txt
#
#I've already left one in: /srv/shiny-server/nrl/data/download_nrl_data

loadLibraries<-function() {
  library(shiny)
  #library(xlsx)
  library(dplyr)
  #library(ggplot2)
  library(plotly)
  library(forecast)
}

suppressWarnings(loadLibraries())

#prepareData.R needs to run first, in order to save manipulate and save the data (this should be a cron job)
load(file = "data/nrlData.RData")

getTeamData<-function(x){
  #Where x is the team mascot name
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

printResults<-function(x)
{
  if(nrow(x)<1){
    return("")
  }
  printOut<-NULL
  printOutComplete<-NULL
  for(row in 1:nrow(x)){
    tempRow<-x[row,]
    printOut<-paste0(tempRow$Home.Team,"(h) ",
                     tempRow$Home.Score,
                     " vs ",
                     tempRow$Away.Team,"  ",
                     tempRow$Away.Score,
                     " <small>(",tempRow$Date,")</small>")
    #Add notes:
    if(!is.na(tempRow$Notes)){
      printOut<-paste0(printOut," Notes: ",tempRow$Notes)
    }
    printOutComplete<-paste(printOut,printOutComplete,sep = "<br/>")
  }
  #cat(printOutComplete)
  printOutComplete
}

shinyServer(function(input, output) {
  #Uncomment the next one at production time
  options(warn = -1)
  output$plot <- renderPlotly({
    
    homeName<-input$homeName
    awayName<-input$awayName
    
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
    #Used ggplot before plotly, left here just in case:
    combinedPlot<-ggplot(data=combinedTeams,aes(x=Date,y=Differential))+
      ggtitle(paste0(homeName," (home) vs ",awayName," (away)"))+ylab("Points Differential")+
      geom_point()+
      geom_hline(aes(yintercept = DifferentialMean),data=combinedTeams,linetype = 3)+
      geom_smooth(method="loess",color = "red")+
      facet_grid(Team ~ .)
    #combinedPlot
    
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
    
  })
  output$pastResults <- renderUI({
    homeName<-input$homeName
    awayName<-input$awayName
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
    #Print:
    printing<-paste(" ",
      "<b>This year:</b>", printResults(earlierThisYear),
      "<b>Last year:</b>", printResults(roundsLastYear),
      "<b>Year before last</b>: ", printResults(roundsYearBeforeLast),sep="<br/>"
    )
    HTML(printing)
    
  })

})
