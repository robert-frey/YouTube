library(tidyverse)
library(baseballr)
#install only if you need to
#install.packages('httr')
library(httr)

# get single day of statcast data
savant <- baseballr::statcast_search(start_date = '2022-09-01',
                                     end_date = '2022-09-01')

# get game_pks for pbp data
pbp <- baseballr::mlb_game_pks("2022-09-01")

# get pbp data
pbp_data <- 1:nrow(pbp) %>% purrr::map_df(function(x) mlb_pbp(pbp$game_pk[x]))

# filter columns down to get play ids
pbp_data <- pbp_data %>% 
  select(game_pk,at_bat_number = atBatIndex, pitch_number = pitchNumber,
         play_id = playId) %>%
  dplyr::mutate(at_bat_number = as.double(at_bat_number),
                at_bat_number = at_bat_number + 1,
                pitch_number = as.double(pitch_number))

# join the savant data and the pbp data to get play ids
both <- left_join(savant,pbp_data,by=c('game_pk','at_bat_number',
                                       'pitch_number')) %>%
  mutate(video_url = ifelse(!is.na(play_id),paste0('https://baseballsavant.mlb.com/sporty-videos?playId=',play_id),play_id))

# do specific example
home_run <- both %>% dplyr::filter(events == "home_run",
                                   player_name == "Rodríguez, Julio")

#display video in your browser
httr::BROWSE(url = home_run$video_url[1])
