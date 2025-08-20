#Install these packages if you do not have them
packages <- c("httr", "rvest", "nflreadr", "xml2","jsonlite","tidyverse")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

#Otherwise load in your libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(nflreadr)
library(xml2)
library(rvest)

source("https://raw.githubusercontent.com/robert-frey/freylytics-playbook/refs/heads/main/resources/ff_scrape.R")

espn <- scrape_espn()

espn_qb <- espn %>% dplyr::filter(pos == "QB")
espn_rb <- espn %>% dplyr::filter(pos == "RB")
espn_wr <- espn %>% dplyr::filter(pos == "WR")
espn_te <- espn %>% dplyr::filter(pos == "TE")

cbs <- scrape_cbs()

cbs_qb <- cbs[[1]] %>% mutate(cbs_projected_fantasy_points_ppr = cbs_projected_fantasy_points_ppr - (cbs_projected_fantasy_points_ppr/GP), GP = GP - 1) %>%
  select(gsis_id, cbs_player_adp, cbs_projected_fantasy_points_ppr)
  
cbs_rb <- cbs[[2]] %>% mutate(cbs_projected_fantasy_points_ppr = cbs_projected_fantasy_points_ppr - (cbs_projected_fantasy_points_ppr/GP), GP = GP - 1) %>%
  select(gsis_id, cbs_player_adp, cbs_projected_fantasy_points_ppr)
cbs_wr <- cbs[[3]] %>% mutate(cbs_projected_fantasy_points_ppr = cbs_projected_fantasy_points_ppr - (cbs_projected_fantasy_points_ppr/GP), GP = GP - 1) %>%
  select(gsis_id, cbs_player_adp, cbs_projected_fantasy_points_ppr)

cbs_te <- cbs[[4]] %>% mutate(cbs_projected_fantasy_points_ppr = cbs_projected_fantasy_points_ppr - (cbs_projected_fantasy_points_ppr/GP), GP = GP - 1) %>%
  select(gsis_id, cbs_player_adp, cbs_projected_fantasy_points_ppr)

nfl <- scrape_nfl()


nfl_qb <- nfl[[1]] %>% left_join(.,players %>% select(player=display_name,pos=position,gsis_id),by=c("player","pos")) %>% select(gsis_id, nfl_adp, nfl_projected_fantasy_points_ppr)
nfl_rb <- nfl[[2]] %>% left_join(.,players %>% select(player=display_name,pos=position,gsis_id),by=c("player","pos")) %>% select(gsis_id, nfl_adp, nfl_projected_fantasy_points_ppr)
nfl_wr <- nfl[[3]] %>% left_join(.,players %>% select(player=display_name,pos=position,gsis_id),by=c("player","pos")) %>% select(gsis_id, nfl_adp, nfl_projected_fantasy_points_ppr)
nfl_te <- nfl[[4]] %>% left_join(.,players %>% select(player=display_name,pos=position,gsis_id),by=c("player","pos")) %>% select(gsis_id, nfl_adp, nfl_projected_fantasy_points_ppr)

##### WATCH YOUTUBE VIDEO FOR REMAINING CODE #####