#
#
#
# Receiver Route Definition Fns
#
# Author: Konner Macias
#

#
## Old test:
### Corner       Flat        Fly        idk     In/Dig        Out       Post      Slant 
# 0.01550354 0.11608439 0.01951824 0.25066200 0.45664987 0.06205689 0.04843256 0.03109251 
#
# Without TE:
### Corner       Flat        Fly        idk     In/Dig        Out       Post      Slant 
# 0.01720470 0.11242492 0.01701702 0.24537037 0.46778028 0.06212462 0.05005005 0.02802803 
#
# surprisingly not that much better

## In/Dig Route
InDigRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left
    if ("S" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("E" %in% plDirs & min(which(plDirs=="S")) < min(which(plDirs == "E")),T,F)
    } else { F }
    
    # south right
  } else if (locType == "SR") {
    if ("S" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("W" %in% plDirs & min(which(plDirs=="S")) < min(which(plDirs == "W")),T,F)
    } else { F }
    
    # north left
  } else if (locType == "NL") {
    if ("N" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("E" %in% plDirs & min(which(plDirs=="N")) < min(which(plDirs == "E")),T,F)
    } else { F }
    
    # north right
  } else if (locType == "NR") {
    if ("N" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("W" %in% plDirs & min(which(plDirs=="N")) < min(which(plDirs == "W")),T,F)
    } else { F }
  }
}

#
## Out Route
OutRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left
    if ("S" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("W" %in% plDirs & min(which(plDirs=="S")) < min(which(plDirs == "W")),T,F)
    } else { F }
    
    # south right
  } else if (locType == "SR") {
    if ("S" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("E" %in% plDirs & min(which(plDirs=="S")) < min(which(plDirs == "E")),T,F)
    } else { F }
    
    # north left
  } else if (locType == "NL") {
    if ("N" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("W" %in% plDirs & min(which(plDirs=="N")) < min(which(plDirs == "W")),T,F)
    } else { F }
    
    # north right
  } else if (locType == "NR") {
    if ("N" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("E" %in% plDirs & min(which(plDirs=="N")) < min(which(plDirs == "E")),T,F)
    } else { F }
  }
}

#
## Slant Route
SlantRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left
    if ("S" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SE" %in% plDirs & !("E" %in% plDirs) & min(which(plDirs=="S")) < min(which(plDirs == "SE")),T,F)
    } else { F }
    
    # south right
  } else if (locType == "SR") {
    if ("S" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SW" %in% plDirs & !("W" %in% plDirs) & min(which(plDirs=="S")) < min(which(plDirs == "SW")),T,F)
    } else { F }
    
    # north left
  } else if (locType == "NL") {
    if ("N" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NE" %in% plDirs & !("E" %in% plDirs) & min(which(plDirs=="N")) < min(which(plDirs == "NE")),T,F)
    } else { F }
    
    # north right
  } else if (locType == "NR") {
    if ("N" %in% df$dirFac) {
      # looks at all directions after the first direction we were searching for
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NW" %in% plDirs & !("W" %in% plDirs) & min(which(plDirs=="N")) < min(which(plDirs == "NW")),T,F)
    } else { F }
  }
}


#
## Curl Route
CurlRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left - turning counterclockwise
    if ("S" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SE" %in% plDirs & "E" %in% plDirs & "NE" %in% plDirs &
               min(which(plDirs=="S")) < min(which(plDirs=="SE")) &
               min(which(plDirs=="SE")) < min(which(plDirs=="E")) &
               min(which(plDirs=="E")) < min(which(plDirs=="NE")), T, F)
    } else { F }
    
    # south right
  } else if (locType == "SR") {
    if ("S" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SW" %in% plDirs & "W" %in% plDirs & "NW" %in% plDirs &
               min(which(plDirs=="S")) < min(which(plDirs=="SW")) &
               min(which(plDirs=="SW")) < min(which(plDirs=="W")) &
               min(which(plDirs=="W")) < min(which(plDirs=="NW")), T, F)
    } else { F }
    
    # north left - turn clock wise
  } else if (locType == "NL") {
    if ("N" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NE" %in% plDirs & "E" %in% plDirs & "SE" %in% plDirs &
               min(which(plDirs=="N")) < min(which(plDirs=="NE")) &
               min(which(plDirs=="NE")) < min(which(plDirs=="E")) &
               min(which(plDirs=="E")) < min(which(plDirs=="SE")), T, F)
    } else { F }
    
    # north right
  } else if (locType == "NR") {
    if ("N" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NW" %in% plDirs & "W" %in% plDirs & "SW" %in% plDirs &
               min(which(plDirs=="N")) < min(which(plDirs=="NW")) &
               min(which(plDirs=="NW")) < min(which(plDirs=="W")) &
               min(which(plDirs=="W")) < min(which(plDirs=="SW")), T, F)
    } else { F }
  }
}


#
## Comeback route
ComebackRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left - turning clockwise
    if ("S" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SW" %in% plDirs & "W" %in% plDirs & "NW" %in% plDirs &
               min(which(plDirs=="S")) < min(which(plDirs=="SW")) &
               min(which(plDirs=="SW")) < min(which(plDirs=="W")) &
               min(which(plDirs=="W")) < min(which(plDirs=="NW")), T, F)
    } else { F }
    
    # south right
  } else if (locType == "SR") {
    if ("S" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SE" %in% plDirs & "E" %in% plDirs & "NE" %in% plDirs &
               min(which(plDirs=="S")) < min(which(plDirs=="SE")) &
               min(which(plDirs=="SE")) < min(which(plDirs=="E")) &
               min(which(plDirs=="E")) < min(which(plDirs=="NE")), T, F)
    } else { F }
    
    # north left - turn counter clockwise
  } else if (locType == "NL") {
    if ("N" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NW" %in% plDirs & "W" %in% plDirs & "SW" %in% plDirs &
               min(which(plDirs=="N")) < min(which(plDirs=="NW")) &
               min(which(plDirs=="NW")) < min(which(plDirs=="W")) &
               min(which(plDirs=="W")) < min(which(plDirs=="SW")), T, F)
    } else { F }
    
    # north right
  } else if (locType == "NR") {
    if ("N" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NE" %in% plDirs & "E" %in% plDirs & "SE" %in% plDirs &
               min(which(plDirs=="N")) < min(which(plDirs=="NE")) &
               min(which(plDirs=="NE")) < min(which(plDirs=="E")) &
               min(which(plDirs=="E")) < min(which(plDirs=="SE")), T, F)
    } else { F }
  }
}

#
## Corner route
CornerRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left
    if ("S" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SW" %in% plDirs & !("W" %in% plDirs) &
          min(which(plDirs=="S")) < min(which(plDirs=="SW")), T, F)
    } else { F }
    
    # south right
  } else if (locType == "SR") {
    if ("S" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="S")):length(df$dirFac)]
      ifelse("SE" %in% plDirs & !("E" %in% plDirs) &
               min(which(plDirs=="S")) < min(which(plDirs=="SE")), T, F)
    } else { F }
    
    # north left
  } else if (locType == "NL") {
    if ("N" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NW" %in% plDirs & !("W" %in% plDirs) &
               min(which(plDirs=="N")) < min(which(plDirs=="NW")), T, F)
    } else { F }
    
    # north right
  } else if (locType == "NR") {
    if ("N" %in% df$dirFac) {
      plDirs <- df$dirFac[min(which(df$dirFac=="N")):length(df$dirFac)]
      ifelse("NE" %in% plDirs & !("E" %in% plDirs) &
               min(which(plDirs=="N")) < min(which(plDirs=="NE")), T, F)
    } else { F }
  }
}

#
## Fly Route
FlyRoute <- function(df, locType) {
  
  if (df$PassLength[1] < 15) return (FALSE)
  
  if (locType == "SL" | locType == "SR") {
    if ("S" %in% df$dirFac){
      fr.df <- as.data.frame(prop.table(table(df$dirFac))['S'])
      colnames(fr.df) <- "freq"
      return (ifelse (fr.df$freq[1] > 0.92, T, F)) ##go bruins
    } else {return (FALSE)}
  }
  
  if (locType == "NL" | locType == "NR") {
    if ("N" %in% df$dirFac){
      fr.df <- as.data.frame(prop.table(table(df$dirFac))['N'])
      colnames(fr.df) <- "freq"
      return (ifelse (fr.df$freq[1] > 0.92, T, F)) ##go bruins
    } else {return (FALSE)}
  }
  
}



#
## Route Handler Fn
routeHandler <- function(df) {
  # make sure position is receiver
  # remove TE for experimentation
  if (df$Position[1] %in% c("WR","TE")) {
    # check which direction offense is heading
    if (df$OffDir[1] == "South") {
      # check left side of field
      if (df$y[1] > 26.65) {
        # determine route type
        if (InDigRoute(df,"SL")) return("In/Dig")
        if (OutRoute(df,"SL")) {
          if (df$PassLength[1] > 8) {return("Out")} else {return ("Flat")} ##8 clap go bruins
        }
        if (SlantRoute(df,"SL")) {
          if (df$PassLength[1] > 15) {return("Post")} else {return ("Slant")}
        }
        if (df$PassLength[1] > 15 & CornerRoute(df, "SL")) return ("Corner")
        if (SlantRoute(df,"SL")) return("Slant")
        if (CurlRoute(df,"SL")) return("Curl")
        if (ComebackRoute(df,"SL")) return("Comeback")
        if (FlyRoute(df, "SL")) return("Fly")
        
        
      } else {
        # player is on right side of field
        if (InDigRoute(df,"SR")) return("In/Dig")
        if (OutRoute(df,"SR")) {
          if (df$PassLength[1] > 8) {return("Out")} else {return ("Flat")} ##8 clap go bruins
        }
        if (SlantRoute(df,"SR")) {
          if (df$PassLength[1] > 15) {return("Post")} else {return ("Slant")}
        }
        if (df$PassLength[1] > 15 & CornerRoute(df, "SR")) return ("Corner")
        if (CurlRoute(df,"SR")) return("Curl")
        if (ComebackRoute(df,"SR")) return("Comeback")
        if (FlyRoute(df, "SR")) return("Fly")
        
      }
    } else {
      # offense direction is north
      # check on far left side of field
      if (df$y[1] > 26.65) {
        if (InDigRoute(df,"NL")) return("In/Dig")
        if (OutRoute(df,"NL")) {
          if (df$PassLength[1] > 8) {return("Out")} else {return ("Flat")} ##8 clap go bruins
        }
        if (SlantRoute(df,"NL")) {
          if (df$PassLength[1] > 15) {return("Post")} else {return ("Slant")}
        }
        if (df$PassLength[1] > 15 & CornerRoute(df, "NL")) return ("Corner")
        if (CurlRoute(df,"NL")) return("Curl")
        if (ComebackRoute(df,"NL")) return("Comeback")
        
      } else {
        # right side
        if (InDigRoute(df,"NR")) return("In/Dig")
        if (OutRoute(df,"NR")) {
          if (df$PassLength[1] > 8) {return("Out")} else {return ("Flat")} ##8 clap go bruins
        }
        if (SlantRoute(df,"NR")) {
          if (df$PassLength[1] > 15) {return("Post")} else {return ("Slant")}
        }
        if (df$PassLength[1] > 15 & CornerRoute(df, "NR")) return ("Corner")
        if (CurlRoute(df,"NR")) return("Curl")
        if (ComebackRoute(df,"NR")) return("Comeback")
        if (FlyRoute(df, "NR")) return("Fly")
        
      }
    }
    return("idk")
  }
  return(NA)
}
