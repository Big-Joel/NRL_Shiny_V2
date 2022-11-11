
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
teams<-c("Broncos","Bulldogs","Cowboys","Dragons",
         "Eels","Knights","Panthers","Raiders",
         "Rabbitohs","Roosters","Sea Eagles","Sharks",
         "Storm","Titans","Tigers","Warriors")

shinyUI(fluidPage(
  # Application title
  titlePanel("NRL Betting Monitor"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
      #            "Number of bins:",
      #            min = 1,
      #            max = 50,
      #            value = 30)
      selectInput(inputId = "homeName", label = strong("Home Team"),
                  choices = teams,
                  selected = "Bulldogs"),
      selectInput(inputId = "awayName", label = strong("Away Team"),
                  choices = teams,
                  selected = "Knights")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot"),
      htmlOutput(outputId = "pastResults")
    )
  )
))
