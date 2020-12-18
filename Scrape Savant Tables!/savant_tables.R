library(baseballr)
library(tidyverse)
library(rvest)
library(xml2)

sc_tables <- readr::read_csv("sc_tables.csv")

scrape_statcast_player_table <- function(player, table_type, type = "hitting") {
  code <- sc_tables %>% #### WATCH YOUTUBE VIDEO FOR CODE #####
  names <- unlist(strsplit(player, " "))
  id <- baseballr::playerid_lookup(last_name = names[2],
                                   first_name = names[1]) %>%
    dplyr::select(mlbam_id) %>% dplyr::top_n(-1) %>% dplyr::pull()
  
  url <- paste0("https://baseballsavant.mlb.com/savant-player/",tolower(names[1]),"-",tolower(names[2]),"-",id,"?stats=statcast-r-",type,"-mlb")
  
  #### Statcast Statistics
  if (table_type == "Statcast Statistics") {
    if (type == "pitching") {
      df <- url %>% xml2::read_html() %>% rvest::html_nodes("#statcast_stats_pitching") %>% 
        rvest::html_node("table") %>% rvest::html_table(trim=T) %>%
        data.frame(stringsAsFactors = F) %>%
        purrr::set_names(c("Season","Pitches","Batted_Balls","Barrels","Barrel_Pct","Avg_EV","Avg_LA","Sweet_Spot_Pct",
                           "xBA","xSLG","wOBA","xwOBA","xwOBAcon","Hard_Hit_Pct","K_Pct","BB_Pct","ERA","xERA")) %>%
        dplyr::mutate(Player = player) %>%
        dplyr::select(Season,Player,everything())
    } else if (type == "hitting") {
      df <- url %>% xml2::read_html() %>% rvest::html_nodes("#statcast_glance_batter") %>% 
        rvest::html_node("table") %>% rvest::html_table(trim=T) %>%
        data.frame(stringsAsFactors = F) %>%
        purrr::set_names(c("Season","Pitches","Batted_Balls","Barrels","Barrel_Pct","Avg_EV","Avg_LA","Sweet_Spot_Pct",
                           "xBA","xSLG","wOBA","xwOBA","xwOBAcon","Hard_Hit_Pct","K_Pct","BB_Pct")) %>%
        dplyr::mutate(Player = player) %>%
        dplyr::select(Season,Player,everything()) 
    } 
  } 
  #### Pitch Tracking Table ####
  else if (table_type == "Pitch Tracking") {
    if (type == "pitching") {
      df <- url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% rvest::html_table(trim = T) %>%
        data.frame(stringsAsFactors = F) %>%
        purrr::set_names(c("Season","Pitch_Type","Pitches","v_RHB","v_LHB","Pitch_Pct",
                           "Avg_Velo","PA","AB","H","1B","2B","3B","HR","SO","BBE","BA",
                           "xBA","SLG","xSLG","wOBA","xwOBA","Avg_EV","Avg_LA","Avg_Spin",
                           "Whiff_Pct","PutAway_Pct")) %>%
        dplyr::mutate(Player = player) %>%
        dplyr::select(Season,Player, everything())
    } else if (type == "hitting") {
      df <- url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% rvest::html_table(trim = T) %>%
        data.frame(stringsAsFactors = F) %>%
        purrr::set_names(c("Season","Pitch_Type","Pitches","Pitch_Pct",
                           "PA","AB","H","1B","2B","3B","HR","SO","BBE","BA",
                           "xBA","SLG","xSLG","wOBA","xwOBA","Avg_EV","Avg_LA",
                           "Whiff_Pct","PutAway_Pct")) %>%
        dplyr::mutate(Player = player) %>%
        dplyr::select(Season,Player, everything()) 
    }
  }
  #### Run Values by Pitch Type ####
  else if (table_type == "Run Values by Pitch Type") {
    df <- url %>% xml2::read_html() %>% rvest::html_nodes(xpath = '//*[(@id = "runValues")]') %>%
      rvest::html_table(trim=T) %>%
      data.frame(stringsAsFactors = F) %>%
      purrr::set_names(c("Season","Pitch_Type","Team","RV_100","Run_Value","Pitches","Pitch_Pct",
                         "PA","BA","SLG","wOBA","Whiff_Pct","K_Pct","PutAway_Pct","xBA","xSLG","xwOBA","Hard_Hit_Pct")) %>%
      dplyr::mutate(Player = player) %>%
      dplyr::select(-Team) %>%
      dplyr::select(Season,Player,everything())
  }
  #### Swing Take ####
  else if (table_type == "Swing Take") {
    df <- url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% .[[2]] %>% 
      rvest::html_node("table") %>% rvest::html_table(trim=T) %>%
      purrr::set_names(c("Season","Team","PA","Pitches","Heart_Runs","Shadow_Runs","Chase_Runs","Waste_Runs","Total_Runs")) %>%
      dplyr::slice(-1) %>%
      dplyr::select(-2) %>%
      dplyr::mutate(Player = player) %>%
      dplyr::select(Season,Player,everything())
  }
  #### Plate Discipline ####
  else if (table_type == "Plate Discipline") {
    df <- url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% rvest::html_table(trim=T) %>% .[[1]] %>%
      data.frame(stringsAsFactors = F) %>% 
      purrr::set_names(c("Season","Pitches","Zone_Pct","Swing_Pct","Zone_Contact_Pct","Chase_Pct","Chase_Contact_Pct",
                         "Edge_Pct","First_Pitch_Strike_Pct","Swing_Pct","Whiff_Pct","Meatball_Pct","Meatball_Swing_Pct")) %>%
      dplyr::mutate(Player = player) %>%
      dplyr::select(Season,Player,everything())
    
  }
  #### Batted Ball Profile ####
  else if (table_type == "Batted Ball Profile") {
    df <- url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% rvest::html_table(trim=T) %>%
      data.frame(stringsAsFactors = F) %>%
      purrr::set_names(c("Season","GB_Pct","FB_Pct","LD_Pct","PU_Pct","Pull_Pct","Straight_Pct","Oppo_Pct","Weak_Pct","Topped_Pct",
                         "Under_Pct","Flare_Burner_Pct","Solid_Pct","Barrel_Pct")) %>%
      dplyr::mutate(Player = player) %>%
      dplyr::select(Season,Player,everything())
  }
  #### Percentile Rankings ####
  else if (table_type == "Percentile Rankings") {
    df <- url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% .[[3]] %>% rvest::html_node("table") %>% 
      rvest::html_table(trim=T) %>% data.frame(stringsAsFactors = F) %>%
      dplyr::mutate(Player = player) %>%
      dplyr::rename(Season = Year) %>%
      dplyr::select(Season,Player, everything())
  }
  #### Expected Home Runs by Park ####
  else if (table_type == "Expected Home Runs by Park") {
    df <-  url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% rvest::html_node("table") %>% rvest::html_table(trim=T) %>%
      data.frame(stringsAsFactors = F) %>%
      purrr::set_names(c("Season","HR","LAA","HOU","OAK","SEA","TEX","TOR","BAL","TB","BOS","NYY","CLE","KC","DET","MIN",
                         "CHW","ARI","LAD","SF","SD","COL","ATL","MIA","NYM","WAS","PHI","MIL","STL","CHC","PIT","CIN")) %>%
      dplyr::mutate(Player = player) %>%
      dplyr::select(Season,Player,everything())
  }
  #### Year to Year Changes ####
  else if (table_type == "Year to Year Changes") {
    df <- url %>% xml2::read_html() %>% rvest::html_nodes(css = code) %>% rvest::html_node("table") %>% rvest::html_table(trim=T) %>%
      data.frame(stringsAsFactors = F) %>%
      purrr::set_names(c("Metric","2015","2016","Delta_1516","Change_1516","2017","Delta_1617","Change_1617","2018","Delta_1718",
                         "Change_1718","2019","Delta_1819","Change_1819","2020","Delta_1920","Change_1920")) %>%
      dplyr::mutate(Player = player) %>%
      dplyr::select(Metric,Player,everything())
  } 
  
  return(df)
}

#Test it Out
scrape_statcast_player_table("Jack Flaherty","Run Values by Pitch Type", type = "pitching")
scrape_statcast_player_table("Paul DeJong","Run Values by Pitch Type", type = "hitting")
