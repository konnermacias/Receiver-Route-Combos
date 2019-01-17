library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse)

df <- read_csv("https://raw.githubusercontent.com/konnermacias/2016-NFL-Regular-Season-vs.-Preseason/master/pre_vs_reg.csv")

playoffDf <- df[(df$playOffDepth > 0),]

playoffDf$playOffDepth <- c("Superbowl", "Divisional", "Wildcard","Conference Finals","Divisional","Divisional","Wildcard","Champs","Wildcard","Wildcard","Conference Finals","Divisional")



nflPlot <- function(Reg, Pre, ttl){
  q <- ggplot(df, aes(x = Reg, y = Pre, color = Color)) + geom_point(aes(text = paste("Team: ",Team), colour = factor(Team)), alpha = 0.75, size = 3) + 
    geom_abline(intercept = 0, color = "red", linetype = "dashed") + scale_color_manual(values = df$Color) + labs( title = paste(ttl," - Regular Season vs. Preseason")) + theme_fivethirtyeight()
  
  ggplotly(q) %>%
    layout(xaxis = list(title = "Regular Season"), yaxis = list(title = "Preseason"), showlegend = FALSE)
}

playoffPlot <- function(Reg, Pre, ttl) {
  p <- ggplot(playoffDf, aes(x = Reg, y = Pre, color = Color, shape = factor(playOffDepth))) + geom_point(aes(shape = factor(playOffDepth), text = paste("Team: ",Team), colour = factor(Team)), alpha = 0.75, size = 3) + 
    scale_shape_identity() + guides(shape = "legend") + scale_shape_manual(name = "",
                                                                           labels = c("Wildcard","Divisional","Conference Finals","Superbowl", "Champs"),
                                                                           values = c(1, 2, 3, 4,5)) +
    geom_abline(intercept = 0, color = "red", linetype = "dashed") + scale_color_manual(values = playoffDf$Color) + labs(title = paste(ttl," - Regular Season vs. Preseason")) + theme_fivethirtyeight()
  
  ggplotly(p, sort = FALSE, labels = c("f","few","fewfew","fewfewfewwe","fewwe")) %>%
    layout(xaxis = list(title = "Regular Season"), yaxis = list(title = "Preseason") )
}