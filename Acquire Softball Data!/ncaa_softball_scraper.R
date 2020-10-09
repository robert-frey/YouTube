#View these files on github#

softball_season_id_lu = read.csv("softball_season_id_lu.csv", stringsAsFactors = F)

updated_master_ncaa_team_lu = read.csv("updated_master_ncaa_team_lu.csv", stringsAsFactors = F)

#This is the libraries you need loaded to run this code
library(rvest)
library(tidyverse)
library(xml2)

#This is the function you run and it is loaded into your library
ncaa_softball_scrape <- function(teamid = 703, year, type = 'batting') {
  
  if (year < 2013) {
    stop('you must provide a year that is equal to or greater than 2013')
  }
  
  else
    if (type == "batting") {
      year_id <- subset(softball_season_id_lu, season == year, select = id)
      type_id <- subset(softball_season_id_lu, season == year, select = batting_id)
      url <- paste0("http://stats.ncaa.org/team/", teamid, "/stats?id=", year_id, "&year_stat_category_id=", type_id)
      data_read <- xml2::read_html(url)
      data <- data_read %>%
        rvest::html_nodes("table") %>%
        .[[3]] %>%
        rvest::html_table(fill = TRUE)
      if ("G" %in% colnames(data)) {
        colnames(data) <- c("Jersey","Player","Yr","Pos","GP","GS","GS_R","G","BA","OBP","SLG","AB",
                            "R","H","2B","3B","TB","HR","IBB","BB","HBP","RBI","SF","SH","K","KL",
                            "DP","GDP","TP","SB","CS","Picked","GO","FO")
      } else {
        colnames(data) <- c("Jersey","Player","Yr","Pos","GP","GS","GS_R","BA","OBP","SLG","AB",
                            "R","H","2B","3B","TB","HR","IBB","BB","HBP","RBI","SF","SH","K","KL",
                            "DP","GDP","TP","SB","CS","Picked","GO","FO") 
      }
      df <- as.data.frame(data)
      df$year <- year
      df$teamid <- teamid
      df <- df %>%
        dplyr::left_join(updated_master_ncaa_team_lu,
                         by = c("teamid" = "school_id", "year" = "year"))
      df <- dplyr::select(df, year, school, conference, division, everything())
      df$Player <- gsub("x ", "", df$Player)
      if (!"RBI2out" %in% names(df)) {
        df$RBI2out <- NA
      }
      
      df <- dplyr::select(df,year,school,conference,division,Jersey,Player,
                          Yr,Pos,GP,GS,BA,OBP,SLG,R,AB,H,`2B`,`3B`,TB,
                          HR,RBI,BB,IBB,HBP,SF,SH,K,DP,CS,Picked,SB,RBI2out,GO,FO,teamid, conference_id) %>% mutate(AB = as.numeric(stringr::str_remove(AB,","))) %>%
        tidyr::replace_na(list(R=0,H=0,AB=0,`2B`=0,`3B`=0,HR=0,RBI=0,BB=0,HBP=0,SF=0,SH=0,K=0,DP=0,CS=0,SB=0,GO=0,FO=0)) %>%
        dplyr::mutate(PA = AB+BB+HBP+SF+SH) %>% 
        dplyr::filter(PA > 0)
    }
  
  else if (type == "pitching") {
    year_id <- subset(softball_season_id_lu, season == year, select = id)
    type_id <- subset(softball_season_id_lu, season == year, select = pitching_id)
    url <- paste0("http://stats.ncaa.org/team/", teamid, "/stats?id=", year_id, "&year_stat_category_id=", type_id)
    data_read <- xml2::read_html(url)
    data <- data_read %>%
      rvest::html_nodes("table") %>%
      .[[3]] %>%
      rvest::html_table(fill = TRUE)
    colnames(data) <- c("Jersey","Player","Yr","Pos","GP","GS_R","App","GS","ERA","IP","HA","RA","ER","BBA",
                        "SO","SHO","BF","P-OAB","2B-A","3B-A","HR-A","CSO","WP","Bk","HB","KL","IBB","CG","Inh Run",
                        "Inh Run Score","SHA","SFA","CIA","GO","FO","W","L","SV")
    df <- as.data.frame(data)
    df$year <- year
    df$teamid <- teamid
    df <- df %>%
      dplyr::left_join(updated_master_ncaa_team_lu, by = c("teamid" = "school_id", "year" = "year"))
    df <- dplyr::select(df, year, school, conference, division, everything())
    df$Player <- gsub("x ", "", df$Player)
    df <- dplyr::select(df, year,school,conference,division,Jersey,Player,
                        Yr,Pos,App,GS,ERA,IP,HA,RA,ER,BBA,SO,SHO,BF,`P-OAB`,
                        `2B-A`,`3B-A`,Bk,`HR-A`,WP,HB,IBB,`Inh Run`,`Inh Run Score`,
                        SHA,SFA,CIA,GO,FO,W,L,SV,KL,CSO,teamid,conference_id) %>% dplyr::mutate(BF = stringr::str_remove(BF,","),
                                                                                                `P-OAB` = stringr::str_remove(`P-OAB`,",")) %>%
      dplyr::mutate_at(vars(BF,`P-OAB`), as.numeric) %>%
      dplyr::mutate_at(vars(teamid,conference_id,year,division,Jersey),as.integer) %>%
      dplyr::filter(BF > 0)
  }
  else {
    year_id <- subset(softball_season_id_lu, season == year, select = id)
    type_id <- subset(softball_season_id_lu, season == year, select = fielding_id)
    url <- paste0("http://stats.ncaa.org/team/", teamid, "/stats?id=", year_id, "&year_stat_category_id=", type_id)
    data_read <- xml2::read_html(url)
    data <- data_read %>%
      rvest::html_nodes("table") %>%
      .[[3]] %>%
      rvest::html_table(fill = TRUE)
    colnames(data) <- c("Jersey","Player","Yr","Pos","GP","GS","GS_R","PO","A","E","FldPct","CI","PB",
                        "SBA","CSB","TC","IDP","TP")
    df <- as.data.frame(data)
    df$year <- year
    df$teamid <- teamid
    df <- df %>%
      dplyr::left_join(updated_master_ncaa_team_lu, by = c("teamid" = "school_id", "year" = "year"))
    df <- dplyr::select(df, year, school, conference, division, everything())
    df$Player <- gsub("x ", "", df$Player)
    df <- dplyr::select(df, year,school,conference,division,Jersey,Player,
                        Yr,Pos,GP,GS,PO,A,E,FldPct,CI,PB,SBA,CSB,TC,IDP,TP,teamid,conference_id)
  }
  
  player_url <- data_read %>%
    html_nodes('#stat_grid a') %>%
    html_attr('href') %>%
    as.data.frame() %>%
    dplyr::rename(player_url = '.') %>%
    dplyr::mutate(player_url = paste0('http://stats.ncaa.org', player_url))
  
  player_names_join <- data_read %>%
    rvest::html_nodes('#stat_grid a') %>%
    rvest::html_text() %>%
    as.data.frame() %>%
    dplyr::rename(player_names_join = '.')
  
  player_id <-
    stringr::str_split(pattern = '&stats_player_seq=',  string = player_url$player_url,simplify = T)[,2] %>%
    as.data.frame() %>%
    dplyr::rename(player_id = '.')
  
  player_url_comb <- bind_cols(player_names_join, player_id, player_url)
  
  df <- df %>% dplyr::left_join(
    player_url_comb, by = c('Player' = 'player_names_join'))
  
  return(df)
  
}

#This is the example, I used the University of Texas' batting in 2020
TX = ncaa_softball_scrape(teamid = 703, year = 2020, type = "batting")
View(TX)

