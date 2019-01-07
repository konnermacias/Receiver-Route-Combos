#
#
#
# Receiver Route Definition Fns
#
# Author: Konner Macias
#

#
## In/Dig Route
InDigRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left
    ifelse("S" %in% df$dirFac & "E"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="S")) < min(which(df$dirFac=="E")), T, F),
           F)
    
    # south right
  } else if (locType == "SR") {
    ifelse("S" %in% df$dirFac & "W"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="S")) < min(which(df$dirFac=="W")), T, F),
           F)
    
    # north left
  } else if (locType == "NL") {
    ifelse("N" %in% df$dirFac & "E"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="N")) < min(which(df$dirFac=="E")),T,F),
           F)
    
    # north right
  } else if (locType == "NR") {
    ifelse("N" %in% df$dirFac & "W"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="N")) < min(which(df$dirFac=="W")),T,F),
           F)
  }
}

#
## Out Route
OutRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left
    ifelse("S" %in% df$dirFac & "W"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="S")) < min(which(df$dirFac=="W")), T, F),
           F)
    
    # south right
  } else if (locType == "SR") {
    ifelse("S" %in% df$dirFac & "E"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="S")) < min(which(df$dirFac=="E")), T, F),
           F)
    
    # north left
  } else if (locType == "NL") {
    ifelse("N" %in% df$dirFac & "W"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="N")) < min(which(df$dirFac=="W")),T,F),
           F)
    
    # north right
  } else if (locType == "NR") {
    ifelse("N" %in% df$dirFac & "E"  %in% df$dirFac,
           ifelse(min(which(df$dirFac=="N")) < min(which(df$dirFac=="E")),T,F),
           F)
  }
}

#
## Slant Route
SlantRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left
    ifelse("S" %in% df$dirFac & "SE" %in% df$dirFac & !("E" %in% df$dirFac),
           ifelse(min(which(df$dirFac=="S")) < min(which(df$dirFac=="SE")), T, F),
           F)
    
    # south right
  } else if (locType == "SR") {
    ifelse("S" %in% df$dirFac & "SW" %in% df$dirFac & !("W" %in% df$dirFac),
           ifelse(min(which(df$dirFac=="S")) < min(which(df$dirFac=="SW")), T, F),
           F)
    
    # north left
  } else if (locType == "NL") {
    ifelse("N" %in% df$dirFac & "NE" %in% df$dirFac & !("E" %in% df$dirFac),
           ifelse(min(which(df$dirFac=="N")) < min(which(df$dirFac=="NE")),T,F),
           F)
    
    # north right
  } else if (locType == "NR") {
    ifelse("N" %in% df$dirFac & "NW" %in% df$dirFac & !("W" %in% df$dirFac),
           ifelse(min(which(df$dirFac=="N")) < min(which(df$dirFac=="NW")),T,F),
           F)
  }
}

#
## Curl Route
CurlRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left - turning counterclockwise
    ifelse("S" %in% df$dirFac & "SE" %in% df$dirFac & "E" %in% df$dirFac & "NE" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="S")) < min(which(df$dirFac=="SE")) &
                     (min(which(df$dirFac=="SE")) < min(which(df$dirFac=="E"))) &
                     (min(which(df$dirFac=="E")) < min(which(df$dirFac=="NE")))), T, F),
           F)
    
    # south right
  } else if (locType == "SR") {
    ifelse("S" %in% df$dirFac & "SW" %in% df$dirFac & "W" %in% df$dirFac & "NW" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="S")) < min(which(df$dirFac=="SW")) &
                     (min(which(df$dirFac=="SW")) < min(which(df$dirFac=="W"))) &
                     (min(which(df$dirFac=="W")) < min(which(df$dirFac=="NW")))), T, F),
           F)
    
    # north left - turn clock wise
  } else if (locType == "NL") {
    ifelse("N" %in% df$dirFac & "NE" %in% df$dirFac & "E" %in% df$dirFac & "SE" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="N")) < min(which(df$dirFac=="NE")) &
                     (min(which(df$dirFac=="NE")) < min(which(df$dirFac=="E"))) &
                     (min(which(df$dirFac=="E")) < min(which(df$dirFac=="SE")))), T, F),
           F)
    
    # north right
  } else if (locType == "NR") {
    ifelse("N" %in% df$dirFac & "NW" %in% df$dirFac & "W" %in% df$dirFac & "SW" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="N")) < min(which(df$dirFac=="NW")) &
                     (min(which(df$dirFac=="NW")) < min(which(df$dirFac=="W"))) &
                     (min(which(df$dirFac=="W")) < min(which(df$dirFac=="SW")))), T, F),
           F)
  }
}

#
## Comeback route
ComebackRoute <- function(df, locType) {
  if (locType == "SL") {
    # south left - turning clockwise
    ifelse("S" %in% df$dirFac & "SW" %in% df$dirFac & "W" %in% df$dirFac & "NW" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="S")) < min(which(df$dirFac=="SW")) &
                     (min(which(df$dirFac=="SW")) < min(which(df$dirFac=="W"))) &
                     (min(which(df$dirFac=="W")) < min(which(df$dirFac=="NW")))), T, F),
           F)
    
    # south right
  } else if (locType == "SR") {
    ifelse("S" %in% df$dirFac & "SE" %in% df$dirFac & "E" %in% df$dirFac & "NE" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="S")) < min(which(df$dirFac=="SE")) &
                     (min(which(df$dirFac=="SE")) < min(which(df$dirFac=="E"))) &
                     (min(which(df$dirFac=="E")) < min(which(df$dirFac=="NE")))), T, F),
           F)
    
    # north left - turn counter clockwise
  } else if (locType == "NL") {
    ifelse("N" %in% df$dirFac & "NW" %in% df$dirFac & "W" %in% df$dirFac & "SW" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="N")) < min(which(df$dirFac=="NW")) &
                     (min(which(df$dirFac=="NW")) < min(which(df$dirFac=="W"))) &
                     (min(which(df$dirFac=="W")) < min(which(df$dirFac=="SW")))), T, F),
           F)
    
    # north right
  } else if (locType == "NR") {
    ifelse("N" %in% df$dirFac & "NE" %in% df$dirFac & "E" %in% df$dirFac & "SE" %in% df$dirFac,
           ifelse((min(which(df$dirFac=="N")) < min(which(df$dirFac=="NE")) &
                     (min(which(df$dirFac=="NE")) < min(which(df$dirFac=="E"))) &
                     (min(which(df$dirFac=="E")) < min(which(df$dirFac=="SE")))), T, F),
           F)
  }
}

#
## Route Handler Fn
routeHandler <- function(df) {
  # make sure position is receiver
  if (df$Position[1] %in% c("WR","TE")) {
    # check which direction offense is heading
    if (df$OffDir[1] == "South") {
      # check left side of field
      if (df$y[1] > 26.65) {
        # determine route type
        if (InDigRoute(df,"SL")) return("In/Dig")
        if (OutRoute(df,"SL")) return("Out")
        if (SlantRoute(df,"SL")) return("Slant")
        if (CurlRoute(df,"SL")) return("Curl")
        if (ComebackRoute(df,"SL")) return("Comeback")
        
      } else {
        # player is on right side of field
        if (InDigRoute(df,"SR")) return("In/Dig")
        if (OutRoute(df,"SR")) return("Out")
        if (SlantRoute(df,"SR")) return("Slant")
        if (CurlRoute(df,"SR")) return("Curl")
        if (ComebackRoute(df,"SR")) return("Comeback")
        
      }
    } else {
      # offense direction is north
      # check on far left side of field
      if (df$y[1] > 26.65) {
        if (InDigRoute(df,"NL")) return("In/Dig")
        if (OutRoute(df,"NL")) return("Out")
        if (SlantRoute(df,"NL")) return("Slant")
        if (CurlRoute(df,"NL")) return("Curl")
        if (ComebackRoute(df,"NL")) return("Comeback")
        
      } else {
        # right side
        if (InDigRoute(df,"NR")) return("In/Dig")
        if (OutRoute(df,"NR")) return("Out")
        if (SlantRoute(df,"NR")) return("Slant")
        if (CurlRoute(df,"NR")) return("Curl")
        if (ComebackRoute(df,"NR")) return("Comeback")
        
      }
    }
  }
  return(NA)
}

