library(plotly)
source("helpers.R")

shinyServer(
  function(input, output){
    output$plot <- renderPlotly({
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