library(tidyverse)
library(xml2)
library(rvest)

#Acceptable team names for the argument#
#BAL,BOS,NYY,TB,TOR,ATL,MIA,NYM,PHI,WSH
#CLE,CWS,DET,KC,MIN,CHC,CIN,MIL,PIT,STL
#HOU,LAA,OAK,SEA,TEX,ARI,COL,LAD,SD,SF

#Acceptable throwing_hand arguments#
#L,R

#Acceptable pitch_type arguments#
#ALL,FF,SI,CH,CU,FC,SL

spin_direction_leaderboard <- function(num_pitches, pitch_type = "ALL",throwing_hand="All",team) {
  if(missing(team)) {
    if (throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=",throwing_hand,"&playerName=&team=&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=&playerName=&team=&csv=true")
    } 
  }
  else {
    if (throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=",throwing_hand,"&playerName=&team=",team,"&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=&playerName=&team=",team,"&csv=true")
    } 
  }
  
 payload <- read_csv(url)
 
 return(payload)
}

#Get all pitchers who threw in at least one pitch
spin_axis_data <- spin_direction_leaderboard(1)

#statcast data set
sc_2020 <- readr::read_csv("statcast_data_2020.csv")

#Defining zone manipulation
sc_2020 <- sc_2020 %>% mutate(zone_loc = case_when(zone == 1 | zone == 2 | zone == 3 ~ "High",
                                                   zone == 4 | zone == 5 | zone == 6 ~ "Medium",
                                                   zone == 7 | zone == 8 | zone == 9 ~ "Low",
                                                   plate_z >= 3.5 ~ "High",
                                                   zone == 13 | zone == 14 ~ "Low",
                                                   plate_z <= 1.6 ~ "Low",
                                                   TRUE ~ NA_character_))

sc_2020 <- left_join(sc_2020,spin_axis_data,by=c("pitcher"="player_id","pitch_name"="api_pitch_name"))

#Creating Plots
sc_2020 %>% dplyr::filter(pitch_type == "SI", zone_loc == "Low") %>% 
  dplyr::group_by(hawkeye_measured_clock_label,p_throws) %>% dplyr::summarise(n_pitches = n(),
                                                                              spin_rate = mean(release_spin_rate,na.rm=T)) %>%
  ggplot() +
  #### WATCH YOUTUBE VIDEO FOR CODE ####
  theme_classic() +
  ggtitle("Spin Direction Distribution on Low Sinkers") +
  facet_wrap(~p_throws)

#Second Plot
sc_2020 %>% dplyr::filter(pitch_type == "SI", zone_loc == "Low") %>% 
  dplyr::group_by(hawkeye_measured_clock_label,p_throws) %>% dplyr::summarise(n_pitches = n(),
                                                                              active_spin = mean(active_spin,na.rm=T)) %>%
  ggplot() +
  ####WATCH YOUTUBE VIDEO FOR CODE###
  theme_classic() +
  ggtitle("Spin Direction Distribution on Low Sinkers",
          subtitle = "Active Spin Percentage by Clock Time") +
  facet_wrap(~p_throws)