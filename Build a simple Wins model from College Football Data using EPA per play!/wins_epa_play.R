#devtools::install_github("sportsdataverse/cfbfastR")
#install.packages('tidyverse')
#install.packages('xml2')
#install.packages('rvest')

library(cfbfastR)
library(tidyverse)
library(xml2)
library(rvest)

#Load in pbp from last five seasons
pbp <- cfbfastR::load_cfb_pbp(2020:2024)

#Load in win loss records from last three seasons
season_ids <- c(18383, 18183, 17960)

scrape_cfb_records <- function(id) {
  
  url <- paste0("https://stats.ncaa.org/season_divisions/",id,"/wl_streaks")
  
  payload <- url %>% xml2::read_html()
  
  table <- payload %>% rvest::html_nodes('table') %>% rvest::html_table(trim = T) %>% .[[1]]
  
  table <- table %>% dplyr::select(team = 2, win_loss = 4) %>%
    dplyr::filter(team != "Team") %>%
    mutate(year = case_when(id == 18383 ~ 2024,
                            id == 18183 ~ 2023,
                            id == 17960 ~ 2022),
           wins = gsub("-.*","",win_loss),
           wins = as.numeric(wins)) %>%
    dplyr::select(year, team, wins)
    
  
  return(table)
  
}

last3 <- 1:length(season_ids) %>% purrr::map_df(function(x) scrape_cfb_records(season_ids[x]))

#Glimpse at CFB pbp
pbp_shortened <- pbp %>% head(1000)

#Filter for FBS Conferences (sorry, thought WAC was still FBS)
d1_confs <- c("American Athletic","Conference USA","Sun Belt","FBS Independents","SEC","Big Ten",
              "ACC","Pac-12","Big 12","Mountain West","Mid-American","Western Athletic")

#Get offensive EPA/play by year
off_epa <- pbp %>%
  dplyr::mutate(play_offense = case_when(pass == 1 ~ 1,
                                         rush == 1 ~ 1,
                                         TRUE ~ 0)) %>%
  dplyr::filter(play_offense == 1, offense_conference %in% d1_confs) %>%
  dplyr::group_by(year,pos_team) %>%
  dplyr::summarise(plays = n(),
                   EPA = sum(EPA,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(off_epa_play = round(EPA/plays,3)) %>%
  dplyr::select(year,team=pos_team,off_epa_play)

#Get defensive EPA/play by year
def_epa <- pbp %>%
  dplyr::mutate(play_defense = case_when(pass == 1 ~ 1,
                                         rush == 1 ~ 1,
                                         TRUE ~ 0)) %>%
  dplyr::filter(play_defense == 1, defense_conference %in% d1_confs) %>%
  dplyr::group_by(year, def_team = def_pos_team) %>%
  dplyr::summarise(plays = n(),
                   def_EPA = sum(def_EPA,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(def_epa_play = round(def_EPA/plays,3)) %>%
  dplyr::select(year,team=def_team,def_epa_play)

#Raw EPA/play by year offense and defense
all <- left_join(off_epa,def_epa,by=c("year","team"))

#Get previous three years of EPA/play data for the 2023 season
epa_off_pred_23 <- pbp %>%
  dplyr::filter(year %in% c(2020,2021,2022)) %>%
  dplyr::mutate(play_offense = case_when(pass == 1 ~ 1,
                                         rush == 1 ~ 1,
                                         TRUE ~ 0)) %>%
  dplyr::filter(play_offense == 1, offense_conference %in% d1_confs) %>%
  dplyr::group_by(pos_team) %>%
  dplyr::summarise(plays = n(),
                   EPA = sum(EPA,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(off_epa_play = round(EPA/plays,3),
                year = 2023) %>%
  dplyr::select(year,team=pos_team,off_epa_play)

epa_def_pred_23 <- pbp %>%
  dplyr::filter(year %in% c(2020,2021,2022)) %>%
  dplyr::mutate(play_defense = case_when(pass == 1 ~ 1,
                                         rush == 1 ~ 1,
                                         TRUE ~ 0)) %>%
  dplyr::filter(play_defense == 1, defense_conference %in% d1_confs) %>%
  dplyr::group_by(def_team = def_pos_team) %>%
  dplyr::summarise(plays = n(),
                   def_EPA = sum(def_EPA,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(def_epa_play = round(def_EPA/plays,3),
                year = 2023) %>%
  dplyr::select(year,team=def_team,def_epa_play)

all_epa_pred_23 <- left_join(epa_off_pred_23,epa_def_pred_23,by=c("year","team"))

#Get previous three years of EPA/play data for the 2024 season
epa_off_pred_24 <- pbp %>%
  dplyr::filter(year %in% c(2023,2021,2022)) %>%
  dplyr::mutate(play_offense = case_when(pass == 1 ~ 1,
                                         rush == 1 ~ 1,
                                         TRUE ~ 0)) %>%
  dplyr::filter(play_offense == 1, offense_conference %in% d1_confs) %>%
  dplyr::group_by(pos_team) %>%
  dplyr::summarise(plays = n(),
                   EPA = sum(EPA,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(off_epa_play = round(EPA/plays,3),
                year = 2024) %>%
  dplyr::select(year,team=pos_team,off_epa_play)

epa_def_pred_24 <- pbp %>%
  dplyr::filter(year %in% c(2023,2021,2022)) %>%
  dplyr::mutate(play_defense = case_when(pass == 1 ~ 1,
                                         rush == 1 ~ 1,
                                         TRUE ~ 0)) %>%
  dplyr::filter(play_defense == 1, defense_conference %in% d1_confs) %>%
  dplyr::group_by(def_team = def_pos_team) %>%
  dplyr::summarise(plays = n(),
                   def_EPA = sum(def_EPA,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(def_epa_play = round(def_EPA/plays,3),
                year = 2024) %>%
  dplyr::select(year,team=def_team,def_epa_play)

############### WATCH YOUTUBE VIDEO FOR REMAINING CODE ##########################
################################################################################
################################################################################# 