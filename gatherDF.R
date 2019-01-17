#
#
#### Starting Script for building dataframe
#

library(tidyverse)

# gather all general game and player data
file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays) 

file.players <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
players.sum <- read_csv(file.players)
players.sum <- players.sum[,c(1,4)]

# function to get a dataframe for an entire game
getGame <- function(game.id) {
  url <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_"
  tail <- ".csv"
  file.tracking <- paste0(url, game.id, tail)
  game.inst <- read_csv(file.tracking)
  game.inst.merged <- game.inst %>% inner_join(games.sum) %>% inner_join(plays.sum) 
  return(game.inst.merged)
}

#g.df <- getGame(2017090700)
g.df <- getGame(2017091004)

# create subset of dataframe with only passes
pplay.ids <- g.df[grepl("pass", g.df$playDescription),]$playId
g.df <- g.df[g.df$playId %in% unique(pplay.ids),]

# Now add additional columns to dataframe

# add position - slow, draws an error message but works?
g.df <- merge(g.df, players.sum, by="nflId", all.x = TRUE)

# define directions
giveDir <- function(dir) {
  ifelse (dir > 330 | dir < 30, "W",
          ifelse(dir >= 30 & dir < 60, "NW",
                 ifelse(dir >= 60 & dir < 120, "N",
                        ifelse(dir >= 120 & dir < 150, "NE",
                               ifelse(dir >= 150 & dir < 210, "E",
                                      ifelse(dir >= 210 & dir < 240, "SE",
                                             ifelse(dir >= 240 & dir < 300, "S", "SW")))))))
}
g.df$dirFac <- unlist(lapply(g.df$dir, giveDir))

# define offensive direction
g.df$OffDir <- NA
for (play.id in unique(g.df$playId)) {
  df <- g.df[g.df$playId==play.id,]
  # determine possesion team
  posTeam <- unique(df$possessionTeam)
  away.ind <- min(which(df$team == "away"))
  home.ind <- min(which(df$team == "home"))
  # compare the x of both starting positions
  g.df[g.df$playId==play.id,]$OffDir <- 
    ifelse(posTeam == unique(df$homeTeamAbbr),
        ifelse(df$x[away.ind] > df$x[home.ind], "North", "South"),
        ifelse(df$x[away.ind] > df$x[home.ind], "South", "North"))
}

# source in Routes.R to bring in all route def fns and route handler
source("routes.R")

# add new route type variable
g.df$RouteType <- NA
for (play.id in unique(g.df$playId)) {
  # gather dataframe for particular play
  pl.df <- g.df[g.df$playId==play.id,]
  # iterate through all players in play
  for (nfl.id in unique(pl.df$nflId)) {
    if (is.na(nfl.id)) {
      next
    } else {
      # create dataframe for a player within this play
      nfl.pl <- pl.df[pl.df$nflId==nfl.id,]
      
      # get subset of dataframe between ball snap and when pass is arrived
      ball.snap.ind <- which(nfl.pl$event=="ball_snap")
      pass.arr.ind <- which(nfl.pl$event=="pass_arrived")
      
      # if there wasn't any error adjust dataframe
      if (length(pass.arr.ind) != 0) {
        nfl.pl <- nfl.pl[ball.snap.ind:pass.arr.ind,]
      }
      # now assign route type based on position
      # route handler from routes.R
      g.df[!is.na(g.df$nflId) & g.df$playId==play.id & g.df$nflId==nfl.id,]$RouteType <-
        routeHandler(nfl.pl)
    }
  }
}
## takes < 2min to get to this point

# create a routes combo column, looks like:
# 1 slant, 2 curl, 2 NA
# 2 comeback, ...
#
# weakness: losses directionality

# iterate through all playIds
# look through each route type for possesion team
# add counts for different route types
# at end, then build string to give as column name
g.df$routeCombo <- NA
for (play.id in unique(g.df$playId)) {
  pl.df <- filter(g.df, playId == play.id)
  # get shortened df of only offensive team players
  if (pl.df$possessionTeam[1] == pl.df$homeTeamAbbr[1]) {
    pl.df <- filter(pl.df, team == "home")
  } else {
    pl.df <- filter(pl.df, team == "away")
    print(nrow(pl.df))
  }
  # gather counts of factors
  print(pl.df$possessionTeam[1])
  routeTable <- as.data.frame(table(pl.df$RouteType)) # change to route type
  print(routeTable)
  if (nrow(routeTable) == 0) {
    #print(pl.df$RouteType)
    #print(routeTable)
    next
  }
  routeTable$Freq <- routeTable$Freq / nrow(filter(pl.df, nflId == pl.df$nflId[1]))
  
  # now build string
  comboStr = ""
  for (row in 1:nrow(routeTable)) {
    print(routeTable)
    print(routeTable$Freq[row])
    print(routeTable$Var1[row])
    if(!is.na(routeTable$Var1[row])) {
      comboStr <- paste0(comboStr, routeTable$Freq[row], " ", routeTable$Var1[row], ",")
      print(comboStr)
    }
  }
  comboStr <- substr(comboStr, 1, nchar(comboStr)-1)
  g.df[g.df$playId==play.id,]$routeCombo <- comboStr
}

# add hash
g.df$hash <- NA
for (play.id in unique(g.df$playId)) {
  pl.df <- filter(g.df, playId == play.id)
  g.df[g.df$playId==play.id,]$hash <- paste0(unique(pl.df$gameId), "-", play.id)
}

# from here dataframe will be set
# The new df should be shortened to only include crucial data
# indexed by playId? that won't work bc duplicates. hmm playId * gameId
# columns to keep (route combo column)

# hash


### Colnames to keep
#
# time[1],	homeTeamAbbr/homeDisplayName, season, week, gameDate
# visitorScoreBefore play etc.
# quarter, GameClock, down, yardstoGo, possesionTeam, yardlineSide, ... through play
# routeCombo, OffDir, hash
#
# EXTRA
# event - add info, offensive qtr, offensive WR

datalist <- list()

i <- 1

for (hash.x in unique(g.df$hash)) {
  df <- filter(g.df, hash == hash.x)
  df <- df[1,]
  datalist[[i]] <- df
  i <- i +1
}

final <- bind_rows(datalist)

