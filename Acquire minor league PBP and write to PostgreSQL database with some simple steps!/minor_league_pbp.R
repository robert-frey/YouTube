#load libs
#install.packages('RPostgres')
library(RPostgres)

library(baseballr)
library(tidyverse)
#install.packages('furrr')
library(furrr)

# figure out which level_ids we need for minor leagues
leagues <- mlb_league(2022)

# From the mlb_league function we observe the minor league season starts on April 5 and
# ends on September 28 so we create a data.frame going through each game
dates <- data.frame(day = rep(seq(as.Date('2022-04-05'), as.Date('2022-09-28'), by = 'days'),
                              times = 1))
# Acquire the minor league game pks to scrape play-by-play
minor_league_game_pk_list <- 1:nrow(dates) %>% purrr::map(function(x) mlb_game_pks(dates$day[x],
                                                                                   level_ids = c(11:14,16)))

ml_game_pks <- minor_league_game_pk_list %>% bind_rows() %>% dplyr::filter(status.codedGameState == "F",
                                                                           !is.na(game_pk)) %>%
  pull(game_pk)

# Plan your multicore session to run parallel processing (please watch YT video to fully understand)
plan("multisession")

# Use purrr safely package to continue to run code in case of an error
safe_pbp <- safely(mlb_pbp)

#Acquire the minor league play-by-play data
ml_pbp <- 1:length(ml_game_pks) %>% furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  map('result') %>% bind_rows()

#Convert to dataframe
ml_pbp <- ml_pbp %>% as.data.frame()

# write to table (please watch video to fully understand overwrite vs append)
dbWriteTable(conn = con, "minor_league_pbp_2022", value = ml_pbp, overwrite = T)

# test to see if the data is in DB
test <- dbGetQuery(conn = con, "SELECT * FROM minor_league_pbp_2022 LIMIT 100")

# Another example SQL query that is written custom in R
test2 <- dbGetQuery(conn = con, 'SELECT AVG("pitchData.startSpeed") AS avg_velo FROM minor_league_pbp_2022 WHERE "details.type.code" = \'FF\'')                    
