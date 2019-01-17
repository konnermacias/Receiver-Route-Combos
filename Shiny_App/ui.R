library(shiny)
library(plotly)


shinyUI(fluidPage(
  titlePanel("Identifying Optimal Receiver-Route Combinations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  label = "Team",
                  choices = c("Arizona Cardinals","Atlanta Falcons", "Baltimore Ravans", "Carolina Panthers", "Chicago Bears",
                              "Cincinnati Bengals","Cleveland Browns","Dallas Cowboys","Denver Broncos","Detroit Lions","Green Bay Packers",
                              "Houston Texans","Indianapolis Colts","Jacksonville Jaguars","Kansas City Chiefs","Los Angeles Charges",
                              "Los Angeles Rams","Miami Dolphins","Minnesota Vikings","New England Patriots","New Orleans Saints",
                              "New York Giants","New York Jets","Oakland Raiders","Philadelphia Eagles","Pittsburgh Steelers","San Francisco 49ers",
                              "Seattle Seahawks","Tampa Bay Buccaneers","Tennessee Titans","Washington Redskins"),
                  selected = "Arizona Cardinals"),
      selectInput("var",
                  label = "Down",
                  choices = c("Downs","1st Down","2nd Down","3rd Down","4th Down"),
                  selected = "Downs"),
      checkboxInput("checkbox","Playoff Teams", value = FALSE),
      width = 3),
    mainPanel(plotlyOutput("plot"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  label = "Team",
                  choices = c("Arizona Cardinals","Atlanta Falcons", "Baltimore Ravans", "Carolina Panthers", "Chicago Bears",
                              "Cincinnati Bengals","Cleveland Browns","Dallas Cowboys","Denver Broncos","Detroit Lions","Green Bay Packers",
                              "Houston Texans","Indianapolis Colts","Jacksonville Jaguars","Kansas City Chiefs","Los Angeles Charges",
                              "Los Angeles Rams","Miami Dolphins","Minnesota Vikings","New England Patriots","New Orleans Saints",
                              "New York Giants","New York Jets","Oakland Raiders","Philadelphia Eagles","Pittsburgh Steelers","San Francisco 49ers",
                              "Seattle Seahawks","Tampa Bay Buccaneers","Tennessee Titans","Washington Redskins"),
                  selected = "Arizona Cardinals"),
      selectInput("var",
                  label = "Down",
                  choices = c("Downs","1st Down","2nd Down","3rd Down","4th Down"),
                  selected = "Downs"),
      checkboxInput("checkbox","Playoff Teams", value = FALSE),
      width=3),
    mainPanel(plotlyOutput("plot"))
    )
))