library(shiny)

shinyUI(fluidPage(
  titlePanel("2016 NFL Regular Season vs. Preseason"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Win %","Total Offense (YPG)","Passing (YPG)",
                              "Passing Attempts Per Game", "Rushing (YPG)",
                              "Rushing Attempts Per Game",
                              "Total Defense (YPG Allowed)"),
                  selected = "Win %"),
      checkboxInput("checkbox","Playoff Teams", value = FALSE),
      width = 3),
    
    mainPanel(plotlyOutput("plot"))
  )
))