library(plotly)
library(plyr)
source("helpers.R")

shinyServer(
  function(input, output){
    
    df <- read.csv("pre_vs_reg.csv", stringsAsFactors = T)
    
    route_count <- c(0,0,0,0,0,0,0,0)
    names(route_count) <- c("In/Dig","Flat","Slant","Corner","Curl","Comeback","Fly","idk")
    
    for (hash in df$hash) {
      s <- strsplit(as.character(hash), ",")
      
      # for each item in list
      for (i in length(s[[1]])) {
        # split up into 2 and Fly for example
        ss <- strsplit(s[[1]][i], " ")
        # index into Fly Count and increment
        # add the first item (number) to count of route type
        route_count[ss[2]] = route_count[ss[2]] + route_count[ss[1]] 
      }
    }
    
    
    
    output$plot <- renderPlotly({
      
      p <- ggplot(dataset(), aes_string(x=unique(input$RouteType), y=input$y)) + geom_point()
      
      
      
      args <- switch(input$var,
                     "Win %" = list(df$RegSeasonPCT, df$PreSeasonPCT, "Win %"),
                     "Total Offense (YPG)" = list(df$RegTO, df$PreTO, "Total Offense (YPG)"),
                     "Passing (YPG)" = list(df$RegPass, df$PrePass, "Passing (YPG)"),
                     "Passing Attempts Per Game" = list(df$RegPassAtt, df$PrePassAtt, "Passing Attempts Per Game"),
                     "Rushing (YPG)" = list(df$RegRush, df$PreRush, "Rushing (YPG)"),
                     "Rushing Attempts Per Game" = list(df$RegRushAtt, df$PreRushAtt, "Rushing Attempts Per Game"),
                     "Total Defense (YPG Allowed)" = list(df$RegDef, df$PreDef, "Total Defense (YPG Allowed)"))
      
      res <- do.call(nflPlot, args)
      
      if (input$checkbox == TRUE){
        args2 <- switch(input$var,
                        "Win %" = list(playoffDf$RegSeasonPCT, playoffDf$PreSeasonPCT, "Win %"),
                        "Total Offense (YPG)" = list(playoffDf$RegTO, playoffDf$PreTO, "Total Offense (YPG)"),
                        "Passing (YPG)" = list(playoffDf$RegPass, playoffDf$PrePass, "Passing (YPG)"),
                        "Passing Attempts Per Game" = list(playoffDf$RegPassAtt, playoffDf$PrePassAtt, "Passing Attempts Per Game"),
                        "Rushing (YPG)" = list(playoffDf$RegRush, playoffDf$PreRush, "Rushing (YPG)"),
                        "Rushing Attempts Per Game" = list(playoffDf$RegRushAtt, playoffDf$PreRushAtt, "Rushing Attempts Per Game"),
                        "Total Defense (YPG Allowed)" = list(playoffDf$RegDef, playoffDf$PreDef, "Total Defense (YPG Allowed)"))
        
        
        res <- do.call(playoffPlot,args2)
      }
      res
    })
  }
)