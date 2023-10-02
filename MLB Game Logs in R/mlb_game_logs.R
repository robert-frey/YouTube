library(baseballr)
library(tidyverse)
library(jsonlite)
library(janitor)

#Create the Function
mlb_game_logs <- function(id,stat_group,year) {
  
  url <- paste0("http://statsapi.mlb.com/api/v1/people/",id,"/stats?stats=gameLog,statSplits&group=",stat_group,"&season=",year,"&language=en")
  
  resp <- url %>% baseballr:::mlb_api_call()
  
  df <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)[[2]][[1]] 
  
  df <- df %>% 
    janitor::clean_names()  %>% 
    tibble()
  
  df
}

#Chadwick Database
chadwick <- chadwick_player_lu()

chadwick <- chadwick %>% filter(mlb_played_last == 2023)

#Adley Rutschman's Game Logs
adley <- mlb_game_logs(668939,"hitting",2023)

#Sonny Gray's Game Logs
gray <- mlb_game_logs(543243,"pitching",2023)

adley <- adley %>% rename(HR = stat_home_runs,
                          SO = stat_strike_outs)
