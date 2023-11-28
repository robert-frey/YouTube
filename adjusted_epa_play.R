library(tidyverse)
library(cfbfastR)

cfb_pbp <- load_cfb_pbp(2023)

sort(colnames(cfb_pbp))

tms <- cfbfastR::load_cfb_teams(fbs_only = T) %>% filter(classification == "fbs")

teams <- tms %>% pull(school)
conferences <- tms %>% pull(conference)

offense <- cfb_pbp %>% filter(fg_inds == 0, punt_play == 0, kickoff_play == 0) %>%
  group_by(pos_team,week,offense_conference,game_id,def_pos_team) %>%
  summarise(plays = n(),
            EPA = sum(EPA,na.rm=T)) %>%
  ungroup() %>% mutate(off_EPA_play = round(EPA/plays,3)) %>%
  select(-EPA) %>%
  rename(off_plays = plays)

defense <- cfb_pbp %>% filter(fg_inds == 0, punt_play == 0, kickoff_play == 0) %>%
  group_by(def_pos_team,week,defense_conference,game_id,pos_team) %>%
  summarise(plays = n(),
            EPA = sum(EPA,na.rm=T)) %>%
  ungroup() %>% mutate(def_EPA_play = round(EPA/plays,3)) %>%
  select(-EPA) %>%
  rename(def_plays = plays)

epa_off_data_conf <- cfb_pbp %>% filter(fg_inds == 0, punt_play == 0, kickoff_play == 0) %>%
  dplyr::group_by(offense_conference) %>%
  dplyr::summarise(off_lg_EPA = mean(EPA,na.rm=T))

epa_def_data_conf <- cfb_pbp %>% filter(fg_inds == 0, punt_play == 0, kickoff_play == 0) %>%
  dplyr::group_by(defense_conference,def_pos_team) %>%
  dplyr::summarise(def_lg_EPA = mean(EPA,na.rm=T))


epa_data <- left_join(offense,epa_off_data_conf,by=c("offense_conference")) %>%
  left_join(.,epa_def_data_conf,by=c("def_pos_team")) %>%
  left_join(.,defense %>% select(-pos_team,-defense_conference),by=c("game_id","def_pos_team")) %>%
  mutate(game_epa = (off_lg_EPA+def_lg_EPA)/2,
         off_adjustment_factor = ifelse(!is.na(game_epa),game_epa-def_lg_EPA,0),
         def_adjustment_factor = ifelse(!is.na(game_epa),game_epa-off_lg_EPA,0),
         opp_adj_off_epa = off_EPA_play + off_adjustment_factor,
         opp_adj_def_epa = def_EPA_play + def_adjustment_factor)

unadjusted_epa_play <- cfb_pbp %>% filter(fg_inds == 0, punt_play == 0, kickoff_play == 0,
                                          pos_team %in% teams) %>%
  group_by(pos_team) %>% summarise(plays = n(),
                                   EPA = sum(EPA,na.rm=T)) %>%
  ungroup() %>% mutate(raw_EPA_play = round(EPA/plays,3)) %>%
  select(pos_team,raw_EPA_play)

off_adj_epa_play <- epa_data %>% filter(pos_team %in% teams) %>%
  mutate(tot_EPA = opp_adj_off_epa * off_plays) %>%
  group_by(pos_team) %>% summarise(plays = sum(off_plays,na.rm=T),
                                   adj_EPA = sum(tot_EPA,na.rm=T)) %>%
  ungroup() %>% mutate(adj_EPA_play = round(adj_EPA/plays,3)) %>%
  left_join(.,unadjusted_epa_play,by="pos_team") %>%
  mutate(diff = adj_EPA_play - raw_EPA_play)

library(gt)
library(gtExtras)

team_info <- cfbfastR::cfbd_team_info(year = 2023) %>% dplyr::select(school,logo)

off_adj_epa_play %>% left_join(.,team_info,by=c("pos_team"="school")) %>%
  select(school=1,7,4:6) %>%
  arrange(-diff) %>% slice(1:5) %>%
  gt() %>%
  gt_img_rows(columns = logo, img_source = "web", height = 50) %>%
  cols_label(logo = "Logo",
             school = "School",
             adj_EPA_play = "Adjusted EPA/play",
             raw_EPA_play = "Raw EPA/play") %>%
  gt_theme_espn() %>%
  tab_header(title = "Biggest Gainers in Opponent Adjusted Offense EPA/play",
             subtitle = "2023 CFB Regular Season") %>%
  opt_table_lines() %>%
  tab_footnote(footnote = "Data: cfbfastR, Table: @RobertFrey40")
