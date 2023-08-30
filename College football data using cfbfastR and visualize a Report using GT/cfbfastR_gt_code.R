library(cfbfastR)
library(tidyverse)
library(gt)

# Get your CFB API Key
Sys.setenv(CFBD_API_KEY = "xxxxxxx")

#Acquire the week "0" play by play data
wk0 <- cfbd_pbp_data(year = 2023, week = 1, epa_wpa = T)

#Preview data
head(wk0)

sort(colnames(wk0))

#get team name and logo for plotting
team_info <- cfbfastR::cfbd_team_info(year = 2023) %>%
  dplyr::select(school,logo)

#Function
pass_report <- function(df, gid) {
  
  df_filt <- df %>% dplyr::filter(game_id == gid) %>%
    dplyr::mutate(third_fourth_down = ifelse(down == 3 | down == 4,1,0),
                  third_fourth_down_success = ifelse(down %in% c(3,4) & success == 1,1,0))
  
  #get logos
  logos <- team_info %>% dplyr::filter(school %in% unique(df_filt$pos_team)) %>%
    tibble::deframe()
  
  #get winner and loser
  game_info <- cfbd_game_info(year = 2023, game_id = gid) %>% select(home_team,home_points,
                                                                     away_team,away_points) %>%
    left_join(.,team_info,by=c("home_team"="school")) %>% rename(home_logo = logo) %>%
    left_join(.,team_info,by=c("away_team"="school")) %>% rename(away_logo = logo) %>%
    mutate(winner = ifelse(home_points>away_points,home_logo,away_logo),
           loser = ifelse(winner == home_logo,away_logo,home_logo),
           win_score = ifelse(winner == home_logo,home_points,away_points),
           lose_score = ifelse(winner == home_logo,away_points,home_points)) %>%
    select(winner,win_score,loser,lose_score)
  
  #get qb passing data
  pass <- df_filt %>% 
    dplyr::filter(pass == 1, !is.na(passer_player_name)) %>%
    dplyr::group_by(player = passer_player_name, team = pos_team) %>%
    dplyr::summarise(plays = sum(play,na.rm=T),
                     EPA_pass = sum(EPA,na.rm=T),
                     third_fourth_down_success = 100*round(sum(third_fourth_down_success)/sum(third_fourth_down),3))
  
  names <- unique(pass$player)
  
  #get qb rushing data
  rush <- df_filt %>% 
    dplyr::filter(rush == 1, rusher_player_name %in% names) %>%
    dplyr::group_by(player = rusher_player_name,team = pos_team) %>%
    dplyr::summarise(rush_plays = sum(play,na.rm=T),
                     EPA_rush = sum(EPA,na.rm=T))
  
  #join data and create a pretty gt table
  df <- left_join(pass,rush,by=c("player","team")) %>%
    mutate(plays = replace_na(plays,0),
           rush_plays = replace_na(rush_plays,0),
           EPA_pass = replace_na(EPA_pass,0),
           EPA_rush = replace_na(EPA_rush,0),
           plays = plays + rush_plays) %>%
    select(Player = player, team, Plays = plays, EPA_pass,EPA_rush,third_fourth_down_success) %>%
    gt(groupname_col = "team") %>%
    text_transform(locations = cells_row_groups(),
                   fn = function(x) {
                     lapply(x,function(x) {
                      gt::html(paste(
                        web_image(
                          url = logos[[x]],
                          height = 50
                        ),
                        x
                      )) 
                     })
                   }) %>%
    fmt_number(columns = c(EPA_pass,EPA_rush), decimals = 2) %>%
    tab_style(style = cell_fill(color = "green"),
              locations = cells_body(columns = c(EPA_pass),
                                     rows = EPA_pass > 0)) %>%
    tab_style(style = cell_fill(color = "red"),
              locations = cells_body(columns = c(EPA_pass),
                                     rows = EPA_pass < 0)) %>%
    tab_style(style = cell_fill(color = "green"),
              locations = cells_body(columns = c(EPA_rush),
                                     rows = EPA_rush > 0)) %>%
    tab_style(style = cell_fill(color = "red"),
              locations = cells_body(columns = c(EPA_rush),
                                     rows = EPA_rush < 0)) %>%
    tab_style(style = cell_fill(color = "green"),
              locations = cells_body(columns = c(third_fourth_down_success),
                                     rows = third_fourth_down_success >= 40)) %>%
    tab_style(style = cell_fill(color = "red"),
              locations = cells_body(columns = c(third_fourth_down_success),
                                     rows = third_fourth_down_success <= 40)) %>%
    cols_label(EPA_pass = "Passing EPA",
               EPA_rush = "Rushing EPA",
               third_fourth_down_success = "3rd/4th Down Success") %>%
    opt_table_font(font = list(google_font(name = "Roboto Condensed"),default_fonts())) %>%
    tab_header(title = "Advanced Quarterback Report", subtitle = html(paste0(web_image(game_info$winner),game_info$win_score," ",web_image(game_info$loser),game_info$lose_score))) %>%
    opt_table_lines() %>%
    tab_footnote(footnote = "Data: cfbfastR, Report: @RobertFrey40")
  
  return(df)
  
  
}

#run the function
pass_report(wk0,401520145)
