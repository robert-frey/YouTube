library(tidyverse)
library(mgcv)
library(nbastatR)

#Double size to acquire shot data
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

#Source data for court plotting
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")

# Get all NBA teams
nba_teams <- nbastatR::nba_teams() %>% dplyr::filter(isNonNBATeam == 0,
                                                    idDivision > 0)

#Acquire shot data
season_shots_list <- 1:nrow(nba_teams) %>%
  purrr::map(function(x) nbastatR::teams_shots(team_ids = nba_teams$idTeam[x],
                                               seasons = 2023), .progress = T)

#Create shot locations + FG% + Points Per Shot
season_shots <- season_shots_list %>% bind_rows() %>%
  mutate(shot_attempt = ifelse(isShotAttempted == T,1,0),
         shot_made = ifelse(isShotMade == T,1,0),
         loc_x = -as.numeric(as.character(locationX))/10,
         loc_y = as.numeric(as.character(locationY))/10 + hoop_center_y,
         pps = case_when(typeShot == "2PT Field Goal" & shot_made == 1 ~ 2,
                         typeShot == "3PT Field Goal" & shot_made == 1 ~ 3,
                         typeShot == "2PT Field Goal" & shot_made == 0 ~ 0,
                         typeShot == "3PT Field Goal" & shot_made == 0 ~ 0,
                         TRUE ~ NA_real_))


season_shot_pct <- season_shots %>% group_by(zoneBasic) %>%
  summarise(makes = sum(shot_made),
            attempts = sum(shot_attempt),
            pps = sum(pps)) %>%
  ungroup() %>% mutate(lg_xfg_pct = round(makes/attempts,3),
                       lg_pps = round(pps/attempts,3)) %>%
  select(zoneBasic,lg_xfg_pct,lg_pps)

#Create GAM function for xFG% and PPS
xfg_aa_gam_fit <- function(d) {
  gam(as.numeric(xfg_pct_aa) ~
        s(loc_x, loc_y),
      data = d)
}

pps_aa_gam_fit <- function(d) {
  gam(as.numeric(pps_aa) ~
        s(loc_x, loc_y),
      data = d)
}

#Create prediction grid
grid_predict <- function(fit){
  grid <- expand.grid(loc_x = seq(-24,24,length=100),
                      loc_y = seq(0,32,length=100))
  grid$lp <- predict(fit,grid, type = "response")
  
  grid
}

#Heatmap
shot_heatmap_chart <- function(player, court_theme = court_themes$dark,
                               type = "PPS") {
  
  if (type == "PPS") {
    p_title <- paste(player,"PPS Above Avg Heatmap")
    plot_lims <- c(-1,1)
    plot_breaks <- c(-1,0,1)
  } else {
    p_title <- paste(player,"xFG% Above Avg Heatmap")
    plot_lims <- c(-0.3,0.3)
    plot_breaks <- c(-0.3,0,0.3)
  }
  
  df <- season_shots %>% dplyr::filter(namePlayer == player)
  
  shot_pct <- df %>% group_by(zoneBasic) %>%
    summarise(makes = sum(shot_made),
              attempts = sum(shot_attempt),
              pps = sum(pps)) %>%
    ungroup() %>% mutate(xfg_pct = round(makes/attempts,3),
                         pps = round(pps/attempts,3)) %>%
    select(zoneBasic,xfg_pct,pps)
  
  if (type == "PPS") {
    df <- df %>% select(-pps) %>%
      left_join(.,shot_pct,by="zoneBasic") %>%
      left_join(.,season_shot_pct,by="zoneBasic") %>%
      mutate(pps_aa = pps - lg_pps,
             xfg_pct_aa = xfg_pct - lg_xfg_pct) %>%
      pps_aa_gam_fit() %>%
      grid_predict()
  } else {
    df <- df %>% select(-pps) %>%
      left_join(.,shot_pct,by="zoneBasic") %>%
      left_join(.,season_shot_pct,by="zoneBasic") %>%
      mutate(pps_aa = pps - lg_pps,
             xfg_pct_aa = xfg_pct - lg_xfg_pct) %>%
      xfg_aa_gam_fit() %>%
      grid_predict()
  }
  
  plot_court() +
    geom_tile(data=df,aes(x=loc_x,y=loc_y,
                          fill = lp)) +
    scale_fill_viridis_c(
      "%     ",
      limits = plot_lims,
      breaks = plot_breaks,
      option = "inferno",
      guide = guide_colorbar(barwidth = 15)
    ) +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc),
              color = court_theme$lines) +
    xlim(-24,24) +
    ylim(0,32) +
    coord_fixed() +
    theme(legend.text = element_text(size = rel(0.6))) +
    ggtitle(label = p_title,
            subtitle = "2022-23 Season")
  
}

#Example
shot_heatmap_chart("Stephen Curry", type = "PPS")
shot_heatmap_chart("LeBron James", type = "xFG%")
