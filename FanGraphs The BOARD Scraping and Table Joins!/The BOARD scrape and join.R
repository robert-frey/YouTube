library(tidyverse)
devtools::install_github("kazutan/tubeplayR", force = T)
library(tubeplayR)

# Check your current working directory
getwd()

# Change Working Directory and Read in Files #
setwd("C:/Users/Robert Frey/Documents/TheBoardData")
files <- list.files(path = getwd(), pattern = ".csv")
temp <- lapply(files, read_csv)

# Pitch Data #
pitch_data <- temp[[4]] %>% select(-Pos,-Org,-`Top 100`,-Age,-FV,-`Org Rk`) %>% rename(pitcher_name = Name)
pitch_data <- pitch_data %>% separate(col = FB, into = c("Current_FB","Future_FB"), sep = "\\/") %>%
  #Separate Current and Future Values
  separate(col = SL, into = c("Current_SL","Future_SL"), sep = "\\/") %>%
  separate(col = CB, into = c("Current_CB","Future_CB"), sep = "\\/") %>%
  separate(col = CH, into = c("Current_CH","Future_CH"), sep = "\\/") %>%
  separate(col = CMD, into = c("Current_CMD","Future_CMD"), sep = "\\/") %>%
  separate(col = Sits, into = c("Min_FB_Range","Max_FB_Range"), sep = "\\-") %>%
  mutate(across(everything(), str_squish)) %>%
  mutate_at(vars(Current_FB:Tops),as.numeric) %>%
  dplyr::filter(!is.na(playerId))

#Hit Data#
hit_data <- temp[[2]] %>% select(-Pos,-Org,-`Top 100`,-Age,-FV, -`Org Rk`) %>% rename(batter_name = Name)
hit_data <- hit_data %>% separate(col = Hit,into = c("Current_Hit","Future_Hit"), sep = "\\/") %>%
  #Separate Current and Future Values
  separate(col = Game,into = c("Current_Game_Power","Future_Game_Power"), sep = "\\/") %>%
  separate(col = Raw,into = c("Current_Raw_Power","Future_Raw_Power"), sep = "\\/") %>%
  separate(col = Spd,into = c("Current_Spd","Future_Spd"), sep = "\\/") %>%
  separate(col = Fld,into = c("Current_Fld","Future_Fld"), sep = "\\/") %>%
  rename(Hard_Hit_Pct = `Hard Hit%`, Max_EV = `Max EV`, Avg_EV = `Avg EV`,
         Bat_Ctrl = `Bat Ctrl`, Pitch_Sel = `Pitch Sel`) %>%
  mutate(across(everything(),str_squish)) %>%
  mutate_at(vars(Current_Hit:Max_EV),as.numeric) %>%
  dplyr::filter(!is.na(playerId))

attribute_data <- temp[[1]] %>% select(-Name,-Pos,-Age)

#Change Names and allow yourself to watch video in R
total_data <- temp[[3]] %>% dplyr::rename(Current_Level = `Current Level`,
                                          Org_Rk = `Org Rk`,
                                          Sign_Yr = `Sign Yr`,
                                          Sign_Mkt = `Sign Mkt`,
                                          Sign_Org = `Sign Org`,
                                          Signed_From = `Signed From`) %>%
  dplyr::mutate(Video = ifelse(is.na(Video),NA,paste0("https://www.youtube.com/watch?v=",Video))) %>%
  dplyr::filter(!is.na(playerId))

#Watch YouTube Video in R Viewer
tubeplayR::tubeplay(url = total_data %>% dplyr::filter(Name == "Wander Franco") %>%
                      dplyr::pull(Video))

#Join tables for hitting and pitching data
hitting_data_all <- left_join(total_data,hit_data,by="playerId") %>% dplyr::filter(!is.na(batter_name))
hitting_data_all <- left_join(hitting_data_all,attribute_data,by="playerId")

pitching_data_all <- left_join(total_data,pitch_data,by="playerId") %>% dplyr::filter(!is.na(pitcher_name))
pitching_data_all <- left_join(pitching_data_all,attribute_data,by="playerId")

pitching_data_all <- pitching_data_all %>% mutate(Date = Sys.Date())

#Example of Bobby Witt Jr.
tubeplayR::tubeplay(url = hitting_data_all %>% dplyr::filter(Name == "Bobby Witt Jr.") %>%
                      dplyr::pull(Video))
