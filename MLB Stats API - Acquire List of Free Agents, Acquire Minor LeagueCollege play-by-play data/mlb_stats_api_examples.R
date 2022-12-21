library(baseballr)
library(tidyverse)

#get sport ids for college and minor league data
leagues <- mlb_league(season = 2022) %>% select(league_id,league_name,sport_id)

#get free agents for one season
mlb_fa <- mlb_people_free_agents(season = 2022)

#get free agents for multiple seasons and see who was most frequent free agents
years <- c(2010:2022)

mlb_fa_all <- 1:length(years) %>% purrr::map_df(function(x) mlb_people_free_agents(years[x]) %>% mutate(fa_year = years[x]))

mlb_fa_all %>% group_by(player_full_name) %>% summarise(years_in_fa = n()) %>% ungroup() %>% arrange(-years_in_fa)

#create a data frame of college dates and a late-July 3 day window of minors
college_games <- data.frame(day = rep(
  seq(as.Date('2022-02-17'), as.Date('2022-05-29'), by = 'days'),
  times = 1
))

minor_league <- data.frame(day = c("2022-07-28","2022-07-29","2022-07-30"))

#Get the Game IDs for minors and college
minor_league_pks <- 1:nrow(minor_league) %>% purrr::map_df(function(x) mlb_game_pks(minor_league$day[x],
                                                                                    level_ids = c(11:16)))

college_pks <- 1:nrow(college_games) %>% purrr::map_df(function(x) mlb_game_pks(college_games$day[x],
                                                                                level_ids = 22))

#sample 10 minor league games for example sake
ml_sample <- minor_league_pks %>% dplyr::filter(!is.na(game_pk)) %>% sample_n(10)

#Create error handling mlb_pbp function
safe_pbp <- safely(mlb_pbp)

#acquire minor league play-by-play data
minor_league_pbp_list <- 1:nrow(ml_sample) %>% purrr::map(function(x) safe_pbp(ml_sample$game_pk[x]))

minor_league_pbp_data <- minor_league_pbp_list %>% map('result') %>% bind_rows()

colnames(minor_league_pbp_data)

#create a spray chart of minor league play-by-play data
ggspraychart(minor_league_pbp_data %>% rename(hc_x = hitData.coordinates.coordX, hc_y = hitData.coordinates.coordY))

#acquire college play-by-play data at major/minor league parks
college_pbp_list <- 1:nrow(college_pks) %>% purrr::map(function(x) safe_pbp(college_pks$game_pk[x]))

college_pbp_data <- college_pbp_list %>% map('result') %>% bind_rows()

sort(unique(college_pbp_data$batting_team))

#show EV ranges by batted ball type
college_pbp_data %>% rename(bb_type = hitData.trajectory, ev = hitData.launchSpeed) %>%
  dplyr::filter(bb_type %in% c("fly_ball","ground_ball","line_drive","popup")) %>%
  group_by(bb_type) %>% summarise(avg_ev = round(mean(ev,na.rm=T),1),
                                  max_ev = round(max(ev,na.rm=T),1),
                                  min_ev = round(min(ev,na.rm=T),1))

#Create a histogram function to potentially removed outlier data
bb_type_histogram <- function(batted_ball) {
  
  df <- college_pbp_data %>% rename(bb_type = hitData.trajectory, ev = hitData.launchSpeed) %>%
    dplyr::filter(bb_type == batted_ball)
  
  plot <- ggplot(df,aes(x=ev)) +
    geom_histogram(aes(y = ..density..),
                   binwidth = 5,
                   color = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666")
  
  
  return(plot)
  
}

bb_type_histogram("fly_ball")
