library(baseballr)
library(tidyverse)
library(teamcolors)

pitcher_ids <- baseballr::fg_pitcher_leaders(2022,2022) %>%
  select(playerid, Name)

player_df <- baseballr::pitcher_game_logs_fg(8700,2022)

rolling_era_function <- function(id) {
  
  df <- baseballr::pitcher_game_logs_fg(playerid = id, year = 2022)
  
  df <- df %>% dplyr::select(PlayerName,Date,Team,IP,ER) %>% arrange(Date) %>%
    mutate(ip_outs = floor(IP)*3, addl_outs = (IP %% 1)*10,
           total_outs = ip_outs + addl_outs)
  
  df <- df %>% group_by(PlayerName) %>% mutate(season_outs = cumsum(total_outs),
                                               ER = cumsum(ER),
                                               ERA = round((ER*9)/(season_outs/3),2))
  
  df <- df %>% mutate(Team = case_when(Team == "ARI" ~ "Arizona Diamondbacks",
                                       Team == "ATL" ~ "Atlanta Braves",
                                       Team == "BAL" ~ "Baltimore Orioles",
                                       Team == "BOS" ~ "Boston Red Sox",
                                       Team == "CHC" ~ "Chicago Cubs",
                                       Team == "CHW" ~ "Chicago White Sox",
                                       Team == "CIN" ~ "Cincinnati Reds",
                                       Team == "CLE" ~ "Cleveland Indians",
                                       Team == "COL" ~ "Colorado Rockies",
                                       Team == "DET" ~ "Detroit Tigers",
                                       Team == "HOU" ~ "Houston Astros",
                                       Team == "KCR" ~ "Kansas City Royals",
                                       Team == "LAA" ~ "Los Angeles Angels",
                                       Team == "LAD" ~ "Los Angeles Dodgers",
                                       Team == "MIA" ~ "Miami Marlins",
                                       Team == "MIL" ~ "Milwaukee Brewers",
                                       Team == "MIN" ~ "Minnesota Twins",
                                       Team == "NYM" ~ "New York Mets",
                                       Team == "NYY" ~ "New York Yankees",
                                       Team == "OAK" ~ "Oakland Athletics",
                                       Team == "PHI" ~ "Philadelphia Phillies",
                                       Team == "PIT" ~ "Pittsburgh Pirates",
                                       Team == "SDP" ~ "San Diego Padres",
                                       Team == "SEA" ~ "Seattle Mariners",
                                       Team == "SFG" ~ "San Francisco Giants",
                                       Team == "STL" ~ "St. Louis Cardinals",
                                       Team == "TBR" ~ "Tampa Bay Rays",
                                       Team == "TEX" ~ "Texas Rangers",
                                       Team == "TOR" ~ "Toronto Blue Jays",
                                       Team == "WSN" ~ "Washington Nationals",
                                       TRUE ~ "Multiple Teams"),
                      Appearance = row_number())
  
  mlb_colors <- teamcolors::teamcolors %>% dplyr::filter(name %in% unique(df$Team))
  
  p <- ggplot(data = df, aes(x=Appearance,y=ERA, group = 1)) +
    geom_line(size = 2, color = unique(mlb_colors$primary)) +
    geom_hline(yintercept = 4, size = 2, color = unique(mlb_colors$secondary)) +
    ggtitle(label = paste('Rolling ERA of',unique(df$PlayerName),'in 2022')) +
    theme_classic()
  
  
  return(p)
  
  
}


rolling_era_function(18525)
