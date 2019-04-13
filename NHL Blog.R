


#################################
#################################
#####                       #####
#####      A.I. Sports      #####
#####                       #####
#####       NHL: Blog       #####
#####                       #####
#################################
#################################









###############################
#####   Load  libraries   ##### 
###############################

library(data.table)
library(tidyverse)
library(magrittr)
library(d3heatmap)






###############################
#####   Load  Datafiles   ##### 
###############################

# Load functions
source('NHL Functions.R')


## data comes from https://www.hockey-reference.com/leagues/NHL_2019.html

# Load Data
nhl_team <- read_csv("Data/nhl_team.csv", skip = 1)
nhl_adv <- read_csv("Data/nhl_adv.csv", skip = 1)



###############################
#####   View  Datafiles   ##### 
###############################

# view first 6 rows
head(nhl_team)


# Rename columns
nhl_team %<>% rename(Team = X2)
nhl_adv %<>% rename(Team = X2)


# get array of playoff teams
playoffTeam <- nhl_team$Team[grep("\\*", nhl_team$Team)]

# filter to only playoff teams
playoffs <- nhl_team %>%
  filter(Team %in% playoffTeam)


# fix team names
playoffs$Team <- convertNHLnames2Abbreviation(playoffs$Team)

# check to make sure the playoff teams are all correct
table(playoffs$Team)




##############################
#####    Plot Heatmap    ##### 
##############################

# change to dataframe and then matrix to rename rows
playoffsMat <- playoffs %>% data.frame()
row.names(playoffsMat) <- playoffsMat$Team

# get rankings for key metrics
playoffsHeat <- playoffsMat %>% select(AvAge, PTS., GF, GA, SRS, SOS, TG.G, PP., PK., S., SV.) %>%
  rename(Age = AvAge,
         `Points%` = PTS.,
         Goals = GF,
         Opp_Goals = GA,
         Rating = SRS,
         Schedule = SOS,
         Goals_pg = TG.G,
         PowerP = PP.,
         Kills = PK.,
         `Shot%` = S.,
         `Save%` = SV.)

# interactive heatmap with 3 clusters
d3heatmap(playoffsHeat, scale = "column", dendrogram = "row", k_row = 3, colors = "Blues")


playoffsHeat

### individual matchups
# get rankings for key metrics
playoffsHeat1 <- playoffsMat %>% 
  filter(Team %in% c("CGY", "COL")) %>%
  select(AvAge, PTS., GF, GA, SRS, SOS, TG.G, PP., PK., S., SV.) %>%
  rename(Age = AvAge,
         `Points%` = PTS.,
         Goals = GF,
         Opp_Goals = GA,
         Rating = SRS,
         #Schedule = SOS,
         Goals_pg = TG.G,
         PowerP = PP.,
         Kills = PK.,
         `Shot%` = S.,
         `Save%` = SV.)

row.names(playoffsHeat1) <- c("CGY", "COL")
# interactive heatmap with 3 clusters
d3heatmap(playoffsHeat1, scale = "column", colors = "Blues")




