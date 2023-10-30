library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)
library(ggimage)

#Get FTN Data
ftn <- nflreadr::load_ftn_charting(season = 2023)

ftn <- ftn %>% dplyr::select(nflverse_game_id,ftn_play_id:is_qb_fault_sack) %>%
  dplyr::rename(game_id = nflverse_game_id, play_id = nflverse_play_id)

#Load in PBP from 2022
nfl_pbp <- nflreadr::load_pbp(2023)

#Join the Datasets together (FTN, PBP)
pbp <- left_join(nfl_pbp,ftn,by=c("game_id","play_id")) %>%
  dplyr::filter(!is.na(ftn_play_id))

#Import Rosters and Teams
rost <- load_rosters(2023) %>% select(gsis_id,full_name,headshot_url)

teams <- load_teams() %>% select(posteam = team_abbr, espn_logo = team_logo_espn)

#Analyze some pre-snap motion data
#Motion% of all plays
motion <- pbp %>% dplyr::mutate(is_motion = as.integer(as.logical(is_motion))) %>%
  filter(special_teams_play == 0) %>%
  group_by(posteam) %>%
  summarise(plays = n(),
            motion = sum(is_motion)) %>%
  ungroup() %>%
  mutate(pct = 100*round(motion/plays,3))

#EPA when team used pre-snap motion
motion_epa <- pbp %>% filter(is_motion == T) %>%
  group_by(posteam) %>%
  summarise(play = n(),
            EPA = sum(epa,na.rm=T)) %>%
  mutate(EPA_play = EPA/play) %>%
  ungroup() %>% select(posteam,EPA,EPA_play)

#Join Datasets
motion <- left_join(motion,motion_epa,by="posteam") %>%
  left_join(.,teams,by="posteam")

#Show plot of team logos and EPA/play by Motion%
ggplot(motion,aes(x=EPA_play,y=pct)) +
  geom_image(aes(image = espn_logo)) +
  geom_vline(xintercept = mean(motion$EPA_play)) +
  geom_hline(yintercept = mean(motion$pct)) +
  theme_classic() +
  xlab("EPA/play") +
  ylab("Motion%") +
  ggtitle("NFL Teams' EPA Per Play by Frequency of Pre-Snap Motion",
         subtitle = "2023 Season")

#Play Action analysis
pa <- pbp %>% dplyr::filter(is_play_action == T, !is.na(passer_player_id),
                            qb_dropback == 1) %>%
  group_by(gsis_id = passer_player_id) %>%
  summarise(plays = n(),
            EPA = sum(epa,na.rm=T)) %>%
  ungroup() %>% mutate(EPA_play = EPA/plays) %>%
  dplyr::filter(plays >= 25) %>%
  left_join(.,rost,by="gsis_id")

#NFL player heads on plot
ggplot(pa,aes(x=EPA_play,y=plays)) +
  geom_image(aes(image = headshot_url)) +
  geom_vline(xintercept = mean(pa$EPA_play)) +
  geom_hline(yintercept = mean(pa$plays)) +
  theme_classic() +
  xlab("EPA/play") +
  ylab("Play Action Plays") +
  ggtitle("QBs EPA per Play Action (min. 25 plays)",
          subtitle = "2023 Season")

#QB Pressure on Third Down analysis
pressure <- pbp %>% dplyr::filter(n_pass_rushers >= 5, !is.na(passer_player_id),
                                  qb_dropback == 1, down == 3) %>%
  group_by(gsis_id = passer_player_id, posteam) %>%
  summarise(plays = n(),
            EPA = sum(epa,na.rm=T),
            sacks = sum(sack)) %>%
  ungroup() %>% mutate(EPA_play = EPA/plays,
                       sack_rate = 100*round(sacks/plays,3)) %>%
  dplyr::filter(plays >= 10) %>%
  left_join(.,rost,by="gsis_id") %>%
  left_join(.,teams,by="posteam")

#Create Logos to fit next to Name
logos <- pressure %>% select(name = full_name,headshot_url) %>%
  deframe()

#GT Table with pretty styling
pressure %>% select(team = espn_logo,name = full_name,plays,EPA_play) %>%
  arrange(-EPA_play) %>%
  slice(1:5) %>%
  gt() %>%
  gt::text_transform(locations = cells_body(columns = "name"),
                     fn = function(x) {
                       lapply(x, function(x) {
                       gt::html(paste(web_image(url = logos[[x]], height = 50), x))
                     })
}) %>% gt_img_rows(columns = team, img_source = "web", height = 50) %>%
  gt_theme_espn() %>%
  fmt_number(columns = c(EPA_play), decimals = 2) %>%
  data_color(columns = c(EPA_play), colors = "Greens") %>%
  cols_label(EPA_play = "EPA/Play",
             team = "Team",
             name = "Name",
             plays = "Plays") %>%
  opt_table_font(font = list(google_font(name = "Roboto Condensed"),default_fonts())) %>%
  tab_header(title = "QBs EPA/Play on 3rd Down Facing 5+ Rushers", subtitle = "2023 Season (min. 10 plays)") %>%
  opt_table_lines() %>%
  tab_footnote(footnote = "Data: nflreadr, Table: @RobertFrey40")
