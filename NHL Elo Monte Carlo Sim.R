### NHL playoff ratings
# Monte Carlo Simulation





#################################
#################################
#####                       #####
#####      A.I. Sports      #####
#####                       #####
#####    NHL Prediction:    #####
#####       Elo Model       #####
#####                       #####
#################################
#################################




# This script will generate probabilities for each team to reach the second round,
# Conference finals, Championship game, and winning the Stanely Cup.




###################################
###   Monte Carlo Simulation   ####
###################################




# This script will generate probabilities for each team to reach the second round,
# Conference finals, Championship game, and winning the Stanely Cup.





##############################
## Load necessary libraries ## 
##############################

library(PlayerRatings)
library(knitr)
library(networkD3)
library(data.table)
library(tidyverse)
library(magrittr)




###################################
##### ALL NECESSARY FUNCTIONS ##### 
###################################

## convert_pct
convert_pct <- function(x)paste(round(100*x, 1), "%", sep="")

## simulate.series
h.simulate.series <- function(team1, team2){
  
  # Extract Ratings for each team in the matchup
  h.rating.1 <- h.ratings[h.ratings$team == team1, "rating"]
  h.rating.2 <- h.ratings[h.ratings$team == team2, "rating"]
  
  # Probabilities. Elo Rating math
  p.1.2 <- 1 / ( 10 ^ ((h.rating.2 - h.rating.1)/400) + 1 )
  p.2.1 <- 1 / ( 10 ^ ((h.rating.1 - h.rating.2)/400) + 1 )
  
  
  # Simulate Series 
  games.1 <- 0    # Initialize games won by Team 1 at 0
  games.2 <- 0    # Initialize games won by Team 2 at 0
  
  while (games.1 < 4 & games.2 < 4){  # while function is a loop until the condition is met. 
    # in this case if one of the teams gets to 4 wins then it's over
    
    # the game result is a random sample of the teams and uses their probabilities of winning
    game.result <- sample(c(team1, team2), size = 1, prob = c(p.1.2, p.2.1), replace = TRUE)
    
    if (game.result==team1){ 
      games.1 <- games.1 + 1 # if team 1 wins that they add 1 to games.1
    } else {
      games.2 <- games.2 + 1 # if team 2 wins that they add 1 to games.2
    }
  }
  
  if (games.1 == 4) {
    winner = team1
  } else {
    winner = team2
  }
  winner
}

## chance.df
h.chance.df <- function(series){
  
  tbl <- table(h.sim.results.df[ , series])
  df <- data.frame(team = names(tbl), chance = as.numeric(tbl)/sum(tbl))
  df <- df[order(df$chance, decreasing=TRUE), ]
  df
}



#############################
#####                   #####
##### SET EVERYTHING UP ##### 
#####                   #####
#############################

# File Name - This is the actual results of the 2019 NHL season
h.file_name <- "Data/NHL2019.csv"

# Set the Seeds
h.east.1a <- "TB"
h.east.2a <- "BOS"
h.east.3a <- "TOR"
h.east.wc2 <- "CBJ"
h.east.1b <- "WSH"
h.east.2b <- "NYI"
h.east.3b <- "PIT"
h.east.wc1 <- "CAR"

h.west.1a <- "CGY"
h.west.2a <- "SJ"
h.west.3a <- "VGK"
h.west.wc2 <- "COL"
h.west.1b <- "NSH"
h.west.2b <- "WPG"
h.west.3b <- "STL"
h.west.wc1 <- "DAL"


#####################################
#####                           #####
##### Read and Process the data #####
#####                           #####
#####################################

# load data
h.data <- read_csv(h.file_name)
# prep data
h.data %<>% select(Date, Visitor, G, Home, G_1) %>%
  rename(date = Date,
         away = Visitor,
         pts.away = G,
         home = Home,
         pts.home = G_1) %>%
  mutate(result = if_else(pts.away > pts.home, 1, 0)) %>%
  select(date, away, home, result)


###########################
#####    Fix Dates    #####
###########################

# The Elo rating function only takes dates in as numeric values. 
# Need to do some manipulation to fix this

# Convert dates to character
h.data$date <- as.character(h.data$date) 
h.new.dates <- c()
for (i in 1:nrow(h.data)){
  h.date.i <- h.data$date[i]
  h.parts.i <- strsplit(h.date.i, "-")
  h.date.i <- paste(h.parts.i[[1]][1], h.parts.i[[1]][2],  h.parts.i[[1]][3], sep=" ")
  h.new.date <- as.Date(h.date.i, "%Y%m%d")
  h.new.dates <- c(h.new.dates, h.new.date)
}

# Add new date format to data frame
h.data$date <- h.new.dates


##############################
#####                    #####    
##### Create ELO Ratings #####
#####                    #####   
##############################


h.ratings.elo <- elo(h.data)
h.ratings <- h.ratings.elo$ratings
h.ratings <- h.ratings[,c("Player", "Rating")]
names(h.ratings) <- c("team","rating")

# change names
h.ratings$team <- convertNHLnames2Abbreviation(h.ratings$team)

## tampa is a huge outlier... need to manually adjust...
h.ratings[h.ratings$team == "TB",] <- 2403.227 - 75

###############################
#####                     #####
##### Run the Simulations ##### 
#####                     #####
###############################

set.seed(12345)

h.simulation.results <- c()

# Set number of simulations at 25,000
num_sims <- 25000
i = 1

while (i <= num_sims){
  
  # Eastern Conference First Round
  series.1 <- h.simulate.series(h.east.1a, h.east.wc2)
  series.2 <- h.simulate.series(h.east.2a, h.east.3a)
  series.3 <- h.simulate.series(h.east.1b, h.east.wc1)
  series.4 <- h.simulate.series(h.east.2b, h.east.3b)
  
  # Western Conference First Round
  series.5 <- h.simulate.series(h.west.1a, h.west.wc2)
  series.6 <- h.simulate.series(h.west.2a, h.west.3a)
  series.7 <- h.simulate.series(h.west.1b, h.west.wc1)
  series.8 <- h.simulate.series(h.west.2b, h.west.3b)
  
  # Eastern Conference Semi-Finals
  series.9 <- h.simulate.series(series.1, series.2)
  series.10 <- h.simulate.series(series.3, series.4)
  
  # Western Conference Semi-Finals
  series.11 <- h.simulate.series(series.5, series.6)
  series.12 <- h.simulate.series(series.7, series.8)
  
  # Eastern Conference Finals
  series.13 <- h.simulate.series(series.9, series.10)
  
  # Western Conference Finals
  series.14 <- h.simulate.series(series.11, series.12)
  
  # Finals
  series.15 <- h.simulate.series(series.13, series.14)
  
  results.all <- c( 
    i,
    series.1, series.2, series.3, series.4,
    series.5, series.6, series.7, series.8,
    series.9, series.10,
    series.11, series.12,
    series.13,
    series.14,
    series.15
  )
  h.simulation.results <- c(h.simulation.results, results.all)
  
  i = i + 1
}

h.sim.results.mat <- matrix(h.simulation.results, ncol=16, byrow=TRUE)
h.sim.results.df <- as.data.frame(h.sim.results.mat)
names(h.sim.results.df) <- c( 
  "sim",
  "series.1", "series.2", "series.3", "series.4",
  "series.5", "series.6", "series.7", "series.8",
  "series.9", "series.10",
  "series.11", "series.12",
  "series.13",
  "series.14",
  "series.15"
)

#################################################
#####                                       #####
##### Create a table with all probabilities ##### 
#####                                       #####
#################################################

# NHL Champions
h.champs.df <- h.chance.df("series.15")

# Conference Champions
h.west.champs.df <- h.chance.df("series.14")
h.east.champs.df <- h.chance.df("series.13")
h.finals <- rbind(h.west.champs.df, h.east.champs.df)

# Conference Finals
h.east.1.df <- h.chance.df("series.9")
h.east.2.df <- h.chance.df("series.10")
h.west.1.df <- h.chance.df("series.11")
h.west.2.df <- h.chance.df("series.12")
h.conf.finals <- rbind(h.east.1.df, h.east.2.df, h.west.1.df, h.west.2.df)

# Second Round
h.east.1.two <- h.chance.df("series.1")
h.east.2.two <- h.chance.df("series.2")
h.east.3.two <- h.chance.df("series.3")
h.east.4.two <- h.chance.df("series.4")
h.west.1.two <- h.chance.df("series.5")
h.west.2.two <- h.chance.df("series.6")
h.west.3.two <- h.chance.df("series.7")
h.west.4.two <- h.chance.df("series.8")
h.round.2.df <- rbind(h.east.1.two, h.east.2.two, h.east.3.two, h.east.4.two,
                      h.west.1.two, h.west.2.two, h.west.3.two, h.west.4.two)

# Merge all probabilities
h.all.chances.df <- merge(h.round.2.df, h.conf.finals, by="team")
names(h.all.chances.df) <- c("team", "round.2", "conf.finals")

h.all.chances.df %<>% 
  left_join(h.finals, by="team") %>%
  rename(finals = chance) %>%
  left_join(h.champs.df, by="team") %>%
  rename(champs = chance) %>%
  arrange(desc(champs), desc(finals), desc(conf.finals), desc(round.2))

# Fix percentages
h.all.chances.df$round.2 <- ifelse(is.na(h.all.chances.df$round.2), 0, h.all.chances.df$round.2)
h.all.chances.df$conf.finals <- ifelse(is.na(h.all.chances.df$conf.finals), 0, h.all.chances.df$conf.finals)
h.all.chances.df$finals <- ifelse(is.na(h.all.chances.df$finals), 0, h.all.chances.df$finals)
h.all.chances.df$champs <- ifelse(is.na(h.all.chances.df$champs), 0, h.all.chances.df$champs)

# going to save this dataframe as is now to plot at the end
sankeyPlotdf <- h.all.chances.df

h.all.chances.df[,2:5] <- sapply(h.all.chances.df[,2:5], convert_pct)

# View results
kable(h.all.chances.df)

# Write to a file
output_filename <- "NHLplayoff_probs_2019.csv"
write.csv(h.all.chances.df, output_filename, row.names=FALSE)



##########################
#####    Plot Sim    #####
##########################


links <- melt(sankeyPlotdf, id = c("team"))
links %<>% rename(source = team,
                  target = variable) 
links$value <- links$value*100

nodes <- data.frame(name =c(as.character(links$source), as.character(links$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

### Sankey Network Plot
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              units = "Percent",
              sinksRight=FALSE)

