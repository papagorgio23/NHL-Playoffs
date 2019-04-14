


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
library(radarchart)






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







###########################

####   Radio Charts   #####

###########################

# get this year averages
radioAvg <- nhl_adv %>%
  summarise(Shooting = median(`S%`), 
            Saves = median(`SV%`), 
            Possession = median(`CF%`),
            Expected_Goals = median(xGF),
            Expected_Goals_Against = median(xGA),
            Goal_Differential = median(axDiff),
            Scoring_Chances = median(SCF),
            Scoring_Chances_Against = median(SCA))

# get standard deviations
radioSD <- nhl_adv %>%
  summarise(Shooting = sd(`S%`), 
            Saves = sd(`SV%`), 
            Possession = sd(`CF%`),
            Expected_Goals = sd(xGF),
            Expected_Goals_Against = sd(xGA),
            Goal_Differential = sd(axDiff),
            Scoring_Chances = sd(SCF),
            Scoring_Chances_Against = sd(SCA))


# filter to only playoff teams
playoffs_adv <- nhl_adv %>%
  filter(Team %in% playoffTeam)

# fix team names
playoffs_adv$Team <- convertNHLnames2Abbreviation(playoffs_adv$Team)

# add z scores
playoffs_adv$Shooting <- zscore(playoffs_adv$`S%`, radioAvg$Shooting, radioSD$Shooting)
playoffs_adv$Saves <- zscore(playoffs_adv$`SV%`, radioAvg$Saves, radioSD$Saves)
playoffs_adv$Possession <- zscore(playoffs_adv$`CF%`, radioAvg$Possession, radioSD$Possession)
playoffs_adv$Expected_Goals <- zscore(playoffs_adv$xGF, radioAvg$Expected_Goals, radioSD$Expected_Goals)
playoffs_adv$Expected_Goals_Against <- zscore(playoffs_adv$xGA, radioAvg$Expected_Goals_Against, radioSD$Expected_Goals_Against)
playoffs_adv$Goal_Differential <- zscore(playoffs_adv$axDiff, radioAvg$Goal_Differential, radioSD$Goal_Differential)
playoffs_adv$Scoring_Chances <- zscore(playoffs_adv$SCF, radioAvg$Scoring_Chances, radioSD$Scoring_Chances)
playoffs_adv$Scoring_Chances_Against <- zscore(playoffs_adv$SCA, radioAvg$Scoring_Chances_Against, radioSD$Scoring_Chances_Against)





### Calgary vs. Colorado 
cgy.col <- playoffs_adv %>% 
  filter(Team %in% c("CGY", "COL")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(cgy.col)
cgy.col <- cbind(metrics, cgy.col)
colnames(cgy.col) <- as.character(unlist(cgy.col[1,]))
cgy.col <- cgy.col[-1,] # remove first row
row.names(cgy.col) <- c() # remove row names
cgy.col$CGY <- as.numeric(as.character(cgy.col$CGY))
cgy.col$COL <- as.numeric(as.character(cgy.col$COL))

# get color matrix for plot
cgy_color <- "#C8102E"
col_color <- "#236192"
colorsplot <- col2rgb(c(cgy_color, col_color))
# plot chart
chartJSRadar(cgy.col, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)





### Tampa Bay vs. Columbus 
tb.cbj <- playoffs_adv %>% 
  filter(Team %in% c("TB", "CBJ")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(tb.cbj)
tb.cbj <- cbind(metrics, tb.cbj)
colnames(tb.cbj) <- as.character(unlist(tb.cbj[1,]))
tb.cbj <- tb.cbj[-1,] # remove first row
row.names(tb.cbj) <- c() # remove row names
tb.cbj$TB <- as.numeric(as.character(tb.cbj$TB))
tb.cbj$CBJ <- as.numeric(as.character(tb.cbj$CBJ))

# get color matrix for plot
tb_color <- "#002868"
cbj_color <- "#CE1126"
colorsplot <- col2rgb(c(tb_color, cbj_color))
# plot chart
chartJSRadar(tb.cbj, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)


### San Jose vs. Vegas 
sj.vgk <- playoffs_adv %>% 
  filter(Team %in% c("SJ", "VGK")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(sj.vgk)
sj.vgk <- cbind(metrics, sj.vgk)
colnames(sj.vgk) <- as.character(unlist(sj.vgk[1,]))
sj.vgk <- sj.vgk[-1,] # remove first row
row.names(sj.vgk) <- c() # remove row names
sj.vgk$SJ <- as.numeric(as.character(sj.vgk$SJ))
sj.vgk$VGK <- as.numeric(as.character(sj.vgk$VGK))

# get color matrix for plot
sj_color <- "#006D75"
vgk_color <- "#B4975A"
colorsplot <- col2rgb(c(sj_color, vgk_color))
# plot chart
chartJSRadar(sj.vgk, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)



### Boston Bruins vs. Toronto Maple Leafs
bos.tor <- playoffs_adv %>% 
  filter(Team %in% c("BOS", "TOR")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(bos.tor)
bos.tor <- cbind(metrics, bos.tor)
colnames(bos.tor) <- as.character(unlist(bos.tor[1,]))
bos.tor <- bos.tor[-1,] # remove first row
row.names(bos.tor) <- c() # remove row names
bos.tor$BOS <- as.numeric(as.character(bos.tor$BOS))
bos.tor$TOR <- as.numeric(as.character(bos.tor$TOR))

# get color matrix for plot
bos_color <- "#FFB81C"
tor_color <- "#003E7E"
colorsplot <- col2rgb(c(bos_color, tor_color))
# plot chart
chartJSRadar(bos.tor, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)



### Nashville Predators vs. Dallas Stars
nsh.dal <- playoffs_adv %>% 
  filter(Team %in% c("NSH", "DAL")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(nsh.dal)
nsh.dal <- cbind(metrics, nsh.dal)
colnames(nsh.dal) <- as.character(unlist(nsh.dal[1,]))
nsh.dal <- nsh.dal[-1,] # remove first row
row.names(nsh.dal) <- c() # remove row names
nsh.dal$NSH <- as.numeric(as.character(nsh.dal$NSH))
nsh.dal$DAL <- as.numeric(as.character(nsh.dal$DAL))

# get color matrix for plot
nsh_color <- "#FFB81C"
dal_color <- "#006847"
colorsplot <- col2rgb(c(nsh_color, dal_color))
# plot chart
chartJSRadar(nsh.dal, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)



### St. Louis Blues vs. Winnipeg Jets
stl.wpg <- playoffs_adv %>% 
  filter(Team %in% c("STL", "WPG")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(stl.wpg)
stl.wpg <- cbind(metrics, stl.wpg)
colnames(stl.wpg) <- as.character(unlist(stl.wpg[1,]))
stl.wpg <- stl.wpg[-1,] # remove first row
row.names(stl.wpg) <- c() # remove row names
stl.wpg$STL <- as.numeric(as.character(stl.wpg$STL))
stl.wpg$WPG <- as.numeric(as.character(stl.wpg$WPG))

# get color matrix for plot
stl_color <- "#002F87"
wpg_color <- "#AC162C"
colorsplot <- col2rgb(c(stl_color, wpg_color))
# plot chart
chartJSRadar(stl.wpg, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)



### Washington Capitals vs. Carolina Hurricanes
wsh.car <- playoffs_adv %>% 
  filter(Team %in% c("WSH", "CAR")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(wsh.car)
wsh.car <- cbind(metrics, wsh.car)
colnames(wsh.car) <- as.character(unlist(wsh.car[1,]))
wsh.car <- wsh.car[-1,] # remove first row
row.names(wsh.car) <- c() # remove row names
wsh.car$WSH <- as.numeric(as.character(wsh.car$WSH))
wsh.car$CAR <- as.numeric(as.character(wsh.car$CAR))

# get color matrix for plot
wsh_color <- "#041E42"
car_color <- "#CC0000"
colorsplot <- col2rgb(c(wsh_color, car_color))
# plot chart
chartJSRadar(wsh.car, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)



### New York Islanders vs. Pittsburgh Penguins
nyi.pit <- playoffs_adv %>% 
  filter(Team %in% c("NYI", "PIT")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(nyi.pit)
nyi.pit <- cbind(metrics, nyi.pit)
colnames(nyi.pit) <- as.character(unlist(nyi.pit[1,]))
nyi.pit <- nyi.pit[-1,] # remove first row
row.names(nyi.pit) <- c() # remove row names
nyi.pit$NYI <- as.numeric(as.character(nyi.pit$NYI))
nyi.pit$PIT <- as.numeric(as.character(nyi.pit$PIT))

# get color matrix for plot
nyi_color <- "#00539B"
pit_color <- "#000000"
colorsplot <- col2rgb(c(nyi_color, pit_color))
# plot chart
chartJSRadar(nyi.pit, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)


### New York Islanders vs. Pittsburgh Penguins
nyi.pit <- playoffs_adv %>% 
  filter(Team %in% c("NYI", "PIT")) %>%
  select(Team, Shooting, Saves, Possession, Expected_Goals, Expected_Goals_Against, Goal_Differential, Scoring_Chances, Scoring_Chances_Against) %>%
  mutate(Expected_Goals_Against = 10 - Expected_Goals_Against,
         Scoring_Chances_Against = 10 - Scoring_Chances_Against) %>%
  t() %>%
  data.frame()

metrics <- row.names(nyi.pit)
nyi.pit <- cbind(metrics, nyi.pit)
colnames(nyi.pit) <- as.character(unlist(nyi.pit[1,]))
nyi.pit <- nyi.pit[-1,] # remove first row
row.names(nyi.pit) <- c() # remove row names
nyi.pit$NYI <- as.numeric(as.character(nyi.pit$NYI))
nyi.pit$PIT <- as.numeric(as.character(nyi.pit$PIT))

# get color matrix for plot
nyi_color <- "#00539B"
pit_color <- "#000000"
colorsplot <- col2rgb(c(nyi_color, pit_color))
# plot chart
chartJSRadar(nyi.pit, maxScale = 10, showToolTipLabel=TRUE, colMatrix = colorsplot)
