



#################################
#################################
#####                       #####
#####      A.I. Sports      #####
#####                       #####
#####     NHL Functions     #####
#####                       #####
#################################
#################################



# function to compute zscores
zscore <- function(value, valueMean, valueSD) {
  score <- (value - valueMean) / valueSD
  score <- round(pnorm(score, 0, 1) *10, 2)
  return(score)
}


#Convert cities/nicknames to team abbreviations
convertNHLnames2Abbreviation <- function(x){
  
  x[grep("Anaheim", x, ignore.case=TRUE)] <- "ANA"
  x[grep("Ducks", x, ignore.case=TRUE)] <- "ANA"
  
  x[grep("Arizona", x, ignore.case=TRUE)] <- "ARI"
  x[grep("Coyotes", x, ignore.case=TRUE)] <- "ARI"
  
  x[grep("Boston", x, ignore.case=TRUE)] <- "BOS"
  x[grep("Bruins", x, ignore.case=TRUE)] <- "BOS"
  
  x[grep("Buffalo", x, ignore.case=TRUE)] <- "BUF"
  x[grep("Sabres", x, ignore.case=TRUE)] <- "BUF"
  
  x[grep("Calgary", x, ignore.case=TRUE)] <- "CGY"
  x[grep("Flames", x, ignore.case=TRUE)] <- "CGY"
  
  x[grep("Carolina", x, ignore.case=TRUE)] <- "CAR"
  x[grep("Hurricanes", x, ignore.case=TRUE)] <- "CAR"
  
  x[grep("Chicago", x, ignore.case=TRUE)] <- "CHI"
  x[grep("Blackhawks", x, ignore.case=TRUE)] <- "CHI"
  
  x[grep("Colorado", x, ignore.case=TRUE)] <- "COL"
  x[grep("Avalanche", x, ignore.case=TRUE)] <- "COL"
  
  x[grep("Columbus", x, ignore.case=TRUE)] <- "CBJ"
  x[grep("Blue Jackets", x, ignore.case=TRUE)] <- "CBJ"
  
  x[grep("Dallas", x, ignore.case=TRUE)] <- "DAL"
  x[grep("Stars", x, ignore.case=TRUE)] <- "DAL"
  
  x[grep("Detroit", x, ignore.case=TRUE)] <- "DET"
  x[grep("Red Wings", x, ignore.case=TRUE)] <- "DET"
  
  x[grep("Edmonton", x, ignore.case=TRUE)] <- "EDM"
  x[grep("Oilers", x, ignore.case=TRUE)] <- "EDM"
  
  x[grep("Florida", x, ignore.case=TRUE)] <- "FLA"
  x[grep("Panthers", x, ignore.case=TRUE)] <- "FLA"
  
  x[grep("Los Angeles", x, ignore.case=TRUE)] <- "LA"
  x[grep("Kings", x, ignore.case=TRUE)] <- "LA"
  
  x[grep("Montreal", x, ignore.case=TRUE)] <- "MTL"
  x[grep("Canadiens", x, ignore.case=TRUE)] <- "MTL"
  
  x[grep("Minnesota", x, ignore.case=TRUE)] <- "MIN"
  x[grep("Wild", x, ignore.case=TRUE)] <- "MIN"
  
  x[grep("Nashville", x, ignore.case=TRUE)] <- "NSH"
  x[grep("Predators", x, ignore.case=TRUE)] <- "NSH"
  
  x[grep("New Jersey", x, ignore.case=TRUE)] <- "NJ"
  x[grep("Devils", x, ignore.case=TRUE)] <- "NJ"
  
  x[grep("New York Islanders", x, ignore.case=TRUE)] <- "NYI"
  x[grep("Islanders", x, ignore.case=TRUE)] <- "NYI"
  
  x[grep("New York Rangers", x, ignore.case=TRUE)] <- "NYR"
  x[grep("Rangers", x, ignore.case=TRUE)] <- "NYR"
  
  x[grep("Ottawa", x, ignore.case=TRUE)] <- "OTT"
  x[grep("Senators", x, ignore.case=TRUE)] <- "OTT"
  
  x[grep("Philadelphia", x, ignore.case=TRUE)] <- "PHI"
  x[grep("Flyers", x, ignore.case=TRUE)] <- "PHI"
  
  x[grep("Pittsburgh", x, ignore.case=TRUE)] <- "PIT"
  x[grep("Penguins", x, ignore.case=TRUE)] <- "PIT"
  
  x[grep("Saint Louis", x, ignore.case=TRUE)] <- "STL"
  x[grep("St Louis", x, ignore.case=TRUE)] <- "STL"
  x[grep("St. Louis", x, ignore.case=TRUE)] <- "STL"
  x[grep("Blues", x, ignore.case=TRUE)] <- "STL"
  
  x[grep("San Jose", x, ignore.case=TRUE)] <- "SJ"
  x[grep("Sharks", x, ignore.case=TRUE)] <- "SJ"
  
  x[grep("Tampa Bay", x, ignore.case=TRUE)] <- "TB"
  x[grep("Lightning", x, ignore.case=TRUE)] <- "TB"
  
  x[grep("Toronto", x, ignore.case=TRUE)] <- "TOR"
  x[grep("Maple Leafs", x, ignore.case=TRUE)] <- "TOR"
  
  x[grep("Vancouver", x, ignore.case=TRUE)] <- "VAN"
  x[grep("Canucks", x, ignore.case=TRUE)] <- "VAN"
  
  x[grep("Vegas", x, ignore.case=TRUE)] <- "VGK"
  x[grep("Golden Knights", x, ignore.case=TRUE)] <- "VGK"
  
  x[grep("Washington", x, ignore.case=TRUE)] <- "WSH"
  x[grep("Capitals", x, ignore.case=TRUE)] <- "WAS"
  
  x[grep("Winnipeg", x, ignore.case=TRUE)] <- "WPG"
  x[grep("Jets", x, ignore.case=TRUE)] <- "WPG"
  
  return(x)
}

