#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(baseballr)
library(tidyverse)
library(xml2)
library(rvest)
library(XML)
library(RCurl)

chadwick <- baseballr::get_chadwick_lu() %>% dplyr::filter(!is.na(key_mlbam),!is.na(key_fangraphs),
                                                           mlb_played_last >= 2020) %>%
    dplyr::mutate(name = paste(name_first,name_last)) %>% dplyr::select(name,key_mlbam,key_fangraphs)

scrape_savant_player_page_pitcher <- function(playerid, table_type = "Statcast") {
    url <- paste0("https://baseballsavant.mlb.com/savant-player/",playerid,"?stats=statcast-r-pitching-mlb")
    name <- url %>% xml2::read_html() %>% rvest::html_nodes(".bio-player-name > div:nth-child(1)") %>% rvest::html_text(trim=T)
    #### Statcast Statistics ####
    if (table_type == "Statcast") {
        table <- url %>% xml2::read_html() %>% html_nodes(xpath = '//*[@id="statcast_stats_pitching"]/table') %>% 
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Season,Player,everything()) %>%
            head(-1)
        colnames(table) <- c("Season","Player","Pitches","Batted_Balls","Barrels","Barrel_Pct","Avg_EV","Max_EV",
                             "Avg_LA","Sweet_Spot_Pct","xBA","xSLG","wOBA","xwOBA","xwOBACON","Hard_Hit_Pct",
                             "K_Pct","BB_Pct", "ERA", "xERA")
    }
    #### Pitch Tracking ####
    else if (table_type == "Pitch Tracking") {
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(xpath = '//*[(@id = "detailedPitches")]') %>%
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Year,Player,everything())
        colnames(table) <- c("Season","Player","Pitch_Type","Pitches","Pitches_vRHB","Pitches_vLHB","Pitch_Pct",
                             "Avg_Velo","PA","AB","H","1B","2B","3B","HR","SO","BBE","BA","xBA","SLG",
                             "xSLG","wOBA", "xwOBA", "Avg_EV","Avg_LA","Avg_Spin_Rate","Whiff_Pct","Put_Away_Pct")
    }
    #else if (table_type == "Movement Profile" | table_type == "Movement") {
    #table <- url %>% xml2::read_html() %>% rvest::html_nodes(css = '.tablesorterb2823c3b5d76d') %>%
    # table <- url %>% xml2::read_html() %>% rvest::html_nodes("table") %>% .[40] %>%
    #  rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>% janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
    # dplyr::mutate(Player = name) %>% dplyr::select(year,Player,everything())
    #colnames(table) <- c("Year","Player","Pitch","Team","Hand","Pitches","Avg_Velo","V_Mov_Drop_Inches",
    #                    "V_Mov_vs_Avg","V_Mov_Percent_vs_Avg","H_Mov_Break_Inches","H_Mov_vs_Avg","H_Mov_Percent_vs_Avg")
    #}
    #### Plate Discipline ####
    else if (table_type == "Plate Discipline" | table_type == "PD" | table_type == "Plate") {
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(xpath = '//*[(@id = "playeDiscipline")]') %>%
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Season,Player,everything(),-Season.1)
        colnames(table) <- c("Season","Player","Pitches","Zone_Pct","Zone_Swing_Pct","Zone_Contact_Pct",
                             "Chase_Pct","Chase_Contact_Pct","Edge_Pct","First_Pitch_Swing_Pct","Swing_Pct",
                             "Whiff_Pct","Meatball_Pct","Meatball_Swing_Pct","GB_Pct","FB_Pct","LD_Pct",
                             "PU_Pct","Pull_Pct","Center_Pct","Oppo_Pct","Weak_Pct","Topped_Pct","Under_Pct",
                             "Flare_Burner_Pct","Solid_Pct","Barrel_Pct")
    }
    return(table)
}

#scrape_savant_player_page_pitcher(594798, table_type = "PD")

scrape_savant_player_page_batter <- function(playerid, table_type = "Statcast") {
    url <- paste0("https://baseballsavant.mlb.com/savant-player/",playerid,"?stats=statcast-r-hitting-mlb")
    name <- url %>% xml2::read_html() %>% rvest::html_nodes(".bio-player-name > div:nth-child(1)") %>% rvest::html_text(trim=T)
    #### Statcast Statistics ####
    if (table_type == "Statcast") {
        table <- url %>% xml2::read_html() %>% html_nodes(xpath = '//*[@id="statcast_glance_batter"]/table') %>% 
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Season,Player,everything()) %>%
            head(-1)
        colnames(table) <- c("Season","Player","Pitches","Batted_Balls","Barrels","Barrel_Pct",
                             "Avg_EV","Max_EV","Avg_LA","Sweet_Spot_Pct","xBA","xSLG","wOBA","xwOBA","xwOBACON",
                             "Hard_Hit_Pct","K_Pct","BB_Pct")
    }
    #### Pitch Tracking ####
    else if (table_type == "Pitch Tracking") {
        table <- url %>% xml2::read_html() %>% html_nodes(xpath = '//*[(@id = "detailedPitches")]') %>% 
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Year,Player,everything())
        colnames(table) <- c("Season","Player","Pitch_Type","Pitches","Pitch_Pct","PA","AB","H","1B",
                             "2B","3B","HR","SO","BBE","BA","xBA","SLG","xSLG","wOBA","xwOBA","Avg_EV","Avg_LA",
                             "Whiff_Pct","Put_Away_Pct")
    }
    #### Plate Discipline ####
    else if (table_type == "Plate Discipline" | table_type == "PD" | table_type == "Plate") {
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(xpath = '//*[(@id = "playeDiscipline")]') %>%
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Season,Player,everything(),-Season.1)
        colnames(table) <- c("Season","Player","Pitches","Zone_Pct","Zone_Swing_Pct","Zone_Contact_Pct",
                             "Chase_Pct","Chase_Contact_Pct","Edge_Pct","First_Pitch_Swing_Pct","Swing_Pct",
                             "Whiff_Pct","Meatball_Pct","Meatball_Swing_Pct","GB_Pct","FB_Pct","LD_Pct",
                             "PU_Pct","Pull_Pct","Center_Pct","Oppo_Pct","Weak_Pct","Topped_Pct","Under_Pct",
                             "Flare_Burner_Pct","Solid_Pct","Barrel_Pct")
    }
    #### Fielding ####
    else if (table_type == "Infield" | table_type == "Infield Defense" | table_type == "Infield OAA") {
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(css = "#statcastFielding") %>% rvest::html_node("table") %>%
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Year,Player,everything(),-Var.5)
        colnames(table) <- c("Season","Player","Team","Pos","OAA","In","Lateral_Toward_3B","Lateral_Toward_1B",
                             "Back","RHB","LHB","Attempts","Success_Rate","Est_Success_Rate","Success_Rate_Added")
    }
    #### Pop Time ####
    else if (table_type == "Pop Time") {
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(css = "#statcastFielding") %>% rvest::html_node("table") %>%
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>% 
            dplyr::mutate(Player = name) %>% select(Var.1,Player,everything()) %>% dplyr::slice(-1)
        colnames(table) <- c("Season","Player","Age","Arm","Exchange","Pop_Time_2B_Att",
                             "Pop_Time_2B_Avg_All","Pop_Time_2B_Avg_CS","Pop_Time_2B_Avg_SB",
                             "Pop_Time_3B_Att","Pop_Time_3B_Avg_All",
                             "Pop_Time_3B_Avg_CS","Pop_Time_3B_Avg_SB")
    }
    #### Catcher Framing ####
    else if (table_type == "Catcher" | table_type == "Framing" | table_type == "Catcher Framing") {
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(css = "#catcher_framing_table") %>% 
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::mutate(Player = name,
                          Strike.Rate = readr::parse_number(Strike.Rate)) %>% dplyr::select(Year,Player,everything(), -Var.13) %>%
            dplyr::mutate_at(vars(contains("Zone")), readr::parse_number)
        colnames(table) <- c("Season","Player","Pitches","Runs_Extra_Strikes","Strike_Rate","Zone_11",
                             "Zone_12","Zone_13","Zone_14","Zone_16","Zone_17","Zone_18",
                             "Zone_19")
    }
    #### Running ####
    else if (table_type == "Running" | table_type == "Sprint Speed") {
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(css = "#statcastRunning") %>% rvest::html_node("table") %>%
            rvest::html_table(fill=T) %>% data.frame(stringsAsFactors = F) %>%
            #dplyr::mutate(Player = name) %>%
            dplyr::select(Season,everything())
        colnames(table) <- c("Season","Age","Sprint_Speed_ft_s","HP_to_1st","Bolts","Pos_Rank",
                             "Age_Rank","League_Rank","Percentile_Rank")
    }
    return(table)
}

scrape_fg_table_pitcher <- function(playerid,table_type = "Original") {
    url = paste0("https://www.fangraphs.com/statss-legacy.aspx?playerid=",playerid,"&position=P")
    projs <- c("ATC","THE BAT","Depth Charts","Steamer","ZiPS","Average")
    name <- url %>% xml2::read_html() %>% rvest::html_nodes("h1") %>% rvest::html_text(trim=T)
    #### Original Dashboard Table ####
    if (table_type == "Original") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason11_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Season,Player,everything()) %>% 
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team",-"LOB%",-"GB%",-"HR/FB"),as.numeric) %>%
            dplyr::mutate_at(vars(contains("%"),"HR/FB"), readr::parse_number) %>% 
            dplyr::mutate_at(vars(contains("%"),"HR/FB"), as.numeric)
    } 
    #### Advanced Stats Table ####
    else if (table_type == "Advanced") {
        table = url %>% xml2::read_html() %>% rvest::html_nodes(css = "table") %>% .[15] %>%
            rvest::html_table(fill = TRUE) %>%
            as.data.frame() %>% dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                                              Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name) %>% dplyr::select(Season,Player,everything())
    } 
    #### ZIPS Projections ####
    else if (table_type == "ZIPS") {
        table = url %>% xml2::read_html() %>% rvest::html_nodes(css = "table") %>% .[16] %>%
            rvest::html_table(fill = TRUE) %>%
            as.data.frame()
    }
    #### Batted Ball Percentages ####
    else if (table_type == "Batted Ball") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason3_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% 
            dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(contains("%"),"HR/FB"), readr::parse_number) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Win Probability ####
    else if (table_type == "Win Probability" | table_type == "WP") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason5_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>% dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Pitch Type ####
    else if (table_type == "Pitch Type") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason22_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>% dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(contains("%")), readr::parse_number) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Pitch Velocity ####
    else if (table_type == "Pitch Velocity") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason25_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>% dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Pitch Values ####
    else if (table_type == "Pitch Values") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason26_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>% dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Pitch Values / 100 ####
    else if (table_type == "Pitch Values/100") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason27_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>% dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Plate Discipline ####
    else if (table_type == "Plate Discipline" | table_type == "PD" | table_type == "Plate") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason7_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything()) %>% 
            dplyr::mutate_at(vars(contains("%")), readr::parse_number) %>%
            dplyr::mutate_at(vars(contains("%")), as.numeric)
    }
    #### Value ####
    else if (table_type == "Value") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason9_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything(),-Salary) %>%
            dplyr::mutate(Dollars = readr::parse_number(Dollars)) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"), as.numeric)
    }
    return(table)
}

#scrape_fg_table_pitcher(10954, table_type = "Value")


##### Scrape FG Batter Data #####
scrape_fg_table_batter <- function(playerid,table_type = "Original") {
    url = paste0("https://www.fangraphs.com/statss-legacy.aspx?playerid=",playerid,"&position=PB")
    projs <- c("ATC","THE BAT","Depth Charts","Steamer","ZiPS","Average","THE BAT X")
    name <- url %>% xml2::read_html() %>% rvest::html_nodes("h1") %>% rvest::html_text(trim=T)
    #### First Table on the FanGraphs Page ####
    if (table_type == "Original") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason11_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything()) %>% 
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team",-"BB%",-"K%"),as.numeric) %>%
            dplyr::mutate_at(vars(contains("%")), readr::parse_number) %>% 
            dplyr::mutate_at(vars(contains("%")), as.numeric)
    } 
    #### Standard Table on Fangraphs Page ####
    else if (table_type == "Standard") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        # select table nodes of interest
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason1_ctl00"]')
        
        # convert HTML tables to data frames
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% 
            dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything()) %>% 
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    } 
    #### Advanced Table on Fangraphs Page ####
    else if (table_type == "Advanced") {
        #url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        #tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason2_ctl00"]')
        
        # table <- #XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
        table <- url %>% xml2::read_html() %>% rvest::html_nodes(xpath = '//*[@id="SeasonStats1_dgSeason2_ctl00"]') %>%
            rvest::html_table(trim=T) %>% data.frame(stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% 
            dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>% dplyr::select(Season,everything()) %>%
            dplyr::rename("BB%"="BB.","K%"="K.","wRC+"="wRC.") %>%
            dplyr::mutate_at(vars(contains("%")), readr::parse_number) %>%
            dplyr::mutate_at(vars(-"Season",-"Team"),as.numeric)
    } 
    #### ZIPS 3-year projections ####
    else if (table_type == "ZIPS" | table_type == "ZiPS" | table_type == "zips") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason29_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::mutate(Player = name) %>% 
            dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(contains("%")), readr::parse_number) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Age"),as.numeric)
    }
    #### Batted Ball Percentages ####
    else if (table_type == "Batted Ball") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason3_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% 
            dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(contains("%"),"HR/FB"), readr::parse_number) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Win Probability ####
    else if (table_type == "Win Probability" | table_type == "WP") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason5_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything()) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"),as.numeric)
    }
    #### Plate Discipline ####
    else if (table_type == "Plate Discipline" | table_type == "PD" | table_type == "Plate") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason7_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything()) %>% 
            dplyr::mutate_at(vars(contains("%")), readr::parse_number) %>%
            dplyr::mutate_at(vars(contains("%")), as.numeric)
    }
    #### Value ####
    else if (table_type == "Value") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason9_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>%       dplyr::select(Season,Player,everything(),-Salary) %>%
            dplyr::mutate(Dollars = readr::parse_number(Dollars)) %>%
            dplyr::mutate_at(vars(-"Season",-"Player",-"Team"), as.numeric)
    }
    ### Advanced Fielding ####
    else if (table_type == "Fielding" | table_type == "Advanced Fielding") {
        url_parsed <- XML::htmlParse(RCurl::getURL(url), asText = TRUE)
        
        tableNodes <- XML::getNodeSet(url_parsed, '//*[@id="SeasonStats1_dgSeason12_ctl00"]')
        
        table <- XML::readHTMLTable(tableNodes[[1]], stringsAsFactors = F) %>%
            dplyr::filter(stringr::str_detect(Team,"\\(")==FALSE,
                          Season != "Postseason", !Team %in% projs) %>% dplyr::distinct(Season,Team,Pos, .keep_all = T) %>%
            dplyr::mutate(Player = name, Season = stringr::str_sub(Season,start = -4), Season = stringr::str_replace(Season,"otal","Total")) %>% 
            dplyr::select(Season,Player,everything())
    }
    return(table)
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MLB Savant and FanGraphs Pages"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        selectInput("Side","Select Hitting or Pitching:",choices = c("Hitting","Pitching"), selected = "Pitching"),
        selectInput("Player","Select Player:",choices = unique(chadwick$name)),
        submitButton(text = "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput("Savant"),
           DT::dataTableOutput("FG")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    sav_table <- reactive({
        savant_id <- chadwick %>% dplyr::filter(name %in% input$Player) %>%
            dplyr::pull(key_mlbam)
        
        if (input$Side == "Hitting") {
            scrape_savant_player_page_batter(savant_id)
        } else {
            scrape_savant_player_page_pitcher(savant_id)
        }
    })
    
    fg_table <- reactive({
        fg_id <- chadwick %>% dplyr::filter(name %in% input$Player) %>%
            dplyr::pull(key_fangraphs)
        
        if (input$Side == "Hitting") {
            scrape_fg_table_batter(fg_id)
        } else {
            scrape_fg_table_pitcher(fg_id)
        }
    })
    
    output$Savant <- DT::renderDataTable({
        datatable(sav_table(), rownames = F, options = list(dom = 't'))
    })
    
    output$FG <- DT::renderDataTable({
        datatable(fg_table(), rownames = F, options = list(dom = 't'))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
