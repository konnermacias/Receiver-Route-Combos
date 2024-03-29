library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse)

df <- read.csv("seven_games.csv", stringsAsFactors = T)

route_count <- c(0,0,0,0,0,0,0,0)
names(route_count) <- c("In/Dig","Flat","Slant","Corner","Curl","Comeback","Fly","idk")

for (hash.id in df$hash) {
  # use hash as a way of diving in
  s <- strsplit(as.character(hash), "-")
  
  routeCombo <- filter(df, hash == hash.id)$routeCombo
  if (is.na(routeCombo))
    next
  ## NOT EVEN USING ROUTECOMBO
  
  # we should get the routeCombo for this hash then parse
  route_ss <- strsplit(routeCombo, ",")
  # for each item in list
  for (i in length(k)) {
    # split up into 2 and Fly for example
    ss <- strsplit(s[[i]][j], " ")
    # index into Fly Count and increment
    # add the first item (number) to count of route type
    ss
    #route_count[[ss[2]]] = route_count[[ss[2]]] + route_count[[ss[1]]] 
  }
}
route_count






playoffDf <- df

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