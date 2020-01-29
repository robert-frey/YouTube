#install nbastatR to scrape shot data
devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(tidyverse)

#Kobe's seasons
seasons = c(1997:2016)

kobeseasons = list()

#loop through seasons and create a data frame of each season of the Lakers
for (i in seq_along(seasons)) {
  kobeseasons[[i]] = nbastatR::teams_shots(teams = "Los Angeles Lakers",
                                           seasons = seasons[i])
}

#load libraries and create one big data frame
library(tidyverse)
library(plyr)
Kobe = ldply(kobeseasons, data.frame)

#filter data frame by Kobe
Kobe = Kobe %>% filter(namePlayer == "Kobe Bryant") %>% select(-slugSeason)

#install SpatialBall Package in R f
#https://derek-corcoran-barrios.github.io/SpatialBall.html
install.packages('SpatialBall')
library(SpatialBall)

#load 2017 season data from SpatialBall package to see what columns are needed
data2017 = season2017

#check column names and match Kobe's to the data2017
colnames(data2017)
colnames(Kobe)

#Change Kobe names
names(Kobe) = c("TEAM_ID","PLAYER_ID","GRID_TYPE","PLAYER_NAME","TEAM_NAME","EVENT_TYPE","ACTION_TYPE",
                "SHOT_TYPE","GAME_DATE","HTM","VTM","GAME_ID","EVENT_ID","PERIOD","MINUTES_REMAINING",
                "SEASON","SHOT_ZONE_BASIC","SHOT_ZONE_AREA","SHOT_ZONE_AREA_ABBR","SHOT_ZONE_RANGE",
                "LOC_X","LOC_Y","SECONDS_REMAINING","SHOT_DISTANCE","SHOT_ATTEMPTED_FLAG","SHOT_MADE_FLAG")

#Convert T/F to 1 and 0 in Kobe data frame
Kobe$SHOT_ATTEMPTED_FLAG = ifelse(Kobe$SHOT_ATTEMPTED_FLAG == TRUE, 1, 0)
Kobe$SHOT_MADE_FLAG = ifelse(Kobe$SHOT_MADE_FLAG == TRUE, 1, 0)

#Kobe's Hex chart expressed as a Percentage
SpatialBall::ShotSeasonGraphPlayer(Kobe, player = "Kobe Bryant", type = "PCT")

#Kobe's Career Made Shots in Reg Season
SpatialBall::PointShotSeasonGraphPlayer(Kobe, player = "Kobe Bryant", Type = "Made", kernel = F) +
  ggtitle("Kobe's Made Shots in Career #RIPMamba")

library(gganimate)

#create animation plot and save file as gif
anim <- SpatialBall::PointShotSeasonGraphPlayer(Kobe, player = "Kobe Bryant", Type = "Made", kernel = F) +
  labs(title = paste("Kobe Bryant's Makes in {closest_state}")) +
  transition_states(SEASON, transition_length = 2, state_length = 2)

anim + ease_aes(y = 'bounce-out') + anim_save(filename = "Kobe.gif")
