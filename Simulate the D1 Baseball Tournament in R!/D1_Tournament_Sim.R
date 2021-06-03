library(tidyverse)
library(baseballr)
#Load in first csv
regional_data <- readr::read_csv("D1Tournament2021.csv")

# function to pull run distribution for each team
ncaa_game_info_catch <- function(team_id, season) {
  school <- combined_data %>% dplyr::filter(teamid == team_id) %>% dplyr::pull(school)
  message(paste0("Acquiring schedule data for ",school))
  test <- tryCatch({baseballr::get_ncaa_schedule_info(teamid = team_id, year = season)}, error=function(e){})
  
  Sys.sleep(sample(seq(.0005,.02,.0001),1))
  
  return(test)
}

#acquire run distribution data and make necessary adjustments
schedule_data <- 1:nrow(regional_data) %>% purrr::map_df(function(x) ncaa_game_info_catch(regional_data$teamid[x],2021) %>% mutate(school = regional_data$school[x]))

schedule_data <- schedule_data %>% separate(score, c("RS", "RA"))

schedule_data <- schedule_data %>% mutate(RS = as.numeric(RS),
                                          RA = as.numeric(RA)) %>% filter(!is.na(RS))

schedule_data <- schedule_data %>% mutate(RS = as.factor(RS),
                                          RA = as.factor(RA))

#get Runs scored and Runs allowed ranges with probabilities for each team
team_run_range <- schedule_data %>% group_by(school,RS) %>%
 dplyr::summarise(RS_value = n()) %>% mutate(RS_Pct = RS_value/sum(RS_value))

team_ra_range <- schedule_data %>% group_by(school,RA) %>%
  dplyr::summarise(RA_value = n()) %>% mutate(RA_Pct = RA_value/sum(RA_value)) 

#sim score function
sim_score <- function(teamA, teamB,reseed) {
  
  if (missing(reseed)) {
  set.seed(111)
  } else {
  set.seed(sample(c(1:100),1))
  }
  
  teamAruns <- team_run_range %>% dplyr::filter(school %in% teamA)
  teamAra <- team_ra_range %>% dplyr::filter(school %in% teamA)
  
  teamA_runs_scored <- sample(as.numeric(teamAruns$RS),1001,replace = T,prob = teamAruns$RS_Pct)
  
  teamA_runs_allowed <- sample(as.numeric(teamAra$RA),1001,replace = T,prob = teamAra$RA_Pct)
  
  teamBruns <- team_run_range %>% dplyr::filter(school %in% teamB)
  teamBra <- team_ra_range %>% dplyr::filter(school %in% teamB)
  
  teamB_runs_scored <- sample(as.numeric(teamBruns$RS),1001,replace = T,prob = teamBruns$RS_Pct)
  
  teamB_runs_allowed <- sample(as.numeric(teamBra$RA),1001,replace = T,prob = teamBra$RA_Pct)
  
  teamAruns <- round((teamA_runs_scored+teamB_runs_allowed)/2,0)
  teamBruns <- round((teamB_runs_scored+teamA_runs_allowed)/2,0)
  
  results <- data.frame(school1 = teamA,school2 = teamB,
                        school1runs = teamAruns, school2runs = teamBruns,
                        winner = case_when(teamAruns > teamBruns ~ teamA,
                                           teamBruns > teamAruns ~ teamB,
                                           TRUE ~ sample(c(teamA,teamB),1)),
                        stringsAsFactors = F)
  
  return(results)
}

#load in other csv for tournament opponents
pairings <- readr::read_csv("D1pairings.csv")

#run first round simulation
output <- 1:nrow(pairings) %>% purrr::map(function(x)
  sim_score(pairings$team1[x],pairings$team2[x]) %>% dplyr::mutate(regional = pairings$Regional[x], no = as.factor(x)) %>%
    dplyr::group_by(winner,regional,no) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

# accurately filter data to have correct 2nd-round regionals
is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

#get winner's bracket data
wb1 <- output %>% filter(Pct > 50.0,is.odd(as.numeric(no))) %>% select(winner,regional) %>% rename(team1 = winner)
wb2 <- output %>% filter(Pct > 50.0,is.even(as.numeric(no))) %>% select(winner,regional) %>% rename(team2 = winner)
winners_bracket <- left_join(wb1,wb2,by="regional") 
rm(wb1,wb2)

#get loser's brakcet data
lb1 <- output %>% filter(Pct < 50.0,is.odd(as.numeric(no))) %>% select(winner,regional) %>% rename(team1 = winner)
lb2 <- output %>% filter(Pct < 50.0,is.even(as.numeric(no))) %>% select(winner,regional) %>% rename(team2 = winner)
losers_bracket <- left_join(lb1,lb2,by="regional")
rm(lb1,lb2)

#2nd round simulation
wb_output <- 1:nrow(winners_bracket) %>% purrr::map(function(x)
  sim_score(winners_bracket$team1[x],winners_bracket$team2[x]) %>% dplyr::mutate(regional = winners_bracket$regional[x]) %>%
    dplyr::group_by(winner,regional) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1)))
#get team that is unfeated in bracket
wb_final <- bind_rows(wb_output) %>% dplyr::filter(Pct > 50.0) %>% select(winner,regional) %>% rename(team1 = winner)

#get loser bracket team
lb_schools <- bind_rows(wb_output) %>% dplyr::filter(Pct < 50.0) %>% select(winner,regional) %>% rename(team2 = winner)

#run loser bracket sim
lb_output <- 1:nrow(losers_bracket) %>% purrr::map(function(x)
  sim_score(losers_bracket$team1[x],losers_bracket$team2[x]) %>% dplyr::mutate(regional = losers_bracket$regional[x]) %>%
    dplyr::group_by(winner,regional) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1)))

#get winner of that game and combine with loser of previous sim
lb_output <- bind_rows(lb_output) %>% dplyr::filter(Pct > 50.0) %>% select(winner,regional) %>% rename(team1 = winner)

lb_output <- left_join(lb_output,lb_schools,by="regional")

#run loser's bracket final to advance to championship
lb_final <- 1:nrow(lb_output) %>% purrr::map(function(x)
  sim_score(lb_output$team1[x],lb_output$team2[x]) %>% dplyr::mutate(regional = lb_output$regional[x]) %>%
    dplyr::group_by(winner,regional) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1)))

lb_final <- bind_rows(lb_final) %>% dplyr::filter(Pct > 50.0) %>% select(winner,regional) %>% rename(team2 = winner)

#get regional final teams
reg_final <- left_join(wb_final,lb_final,by="regional")

undefeated_teams <- reg_final$team1

#run regional final sim
reg_final_1 <- 1:nrow(reg_final) %>% purrr::map(function(x)
  sim_score(reg_final$team1[x],reg_final$team2[x]) %>% dplyr::mutate(regional = reg_final$regional[x]) %>%
    dplyr::group_by(winner,regional) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1)))

#remove teams that went undefeated in regional and advanced to final
reg_final_1 <- bind_rows(reg_final_1) %>% dplyr::filter(Pct > 50.0) %>% select(winner,regional)

reg_final_1 <- reg_final_1 %>% filter(winner %in% undefeated_teams)

#run regional final sim again if necessary
reg_final_2 <- anti_join(reg_final,reg_final_1,by=c("team1"="winner"))

reg_final_2 <- 1:nrow(reg_final_2) %>% purrr::map(function(x)
  #make sure reseed = T
  sim_score(reg_final_2$team1[x],reg_final_2$team2[x], reseed = T) %>% dplyr::mutate(regional = reg_final_2$regional[x]) %>%
    dplyr::group_by(winner,regional) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% dplyr::filter(Pct > 50.0) %>% select(winner,regional)

#get regional champs
reg_champs <- bind_rows(reg_final_1,reg_final_2)

#arrange supers by the correct regional
supers_1 <- data.frame(regional = c("Fayetteville","Stanford","Tucson","Greenville","Austin","South Bend","Fort Worth","Eugene"),
                       no = c(1:8), stringsAsFactors = F)
supers_2 <- data.frame(regional = c("Ruston","Lubbock","Oxford","Nashville","Gainsville","Starkville","Columbia","Knoxville"),
                       no = c(1:8), stringsAsFactors = F)

supers_1 <- left_join(supers_1,reg_champs,by="regional") %>% arrange(no) %>% rename(team1 = winner)

supers_2 <- left_join(supers_2,reg_champs,by="regional") %>% arrange(no) %>% rename(team2 = winner)

# get supers data
supers <- left_join(supers_1,supers_2,by="no")

#run first 2 super regional game sims
supers_1 <- 1:nrow(supers) %>% purrr::map(function(x)
  sim_score(supers$team1[x],supers$team2[x]) %>% dplyr::mutate(no = supers$no[x]) %>%
    dplyr::group_by(winner,no) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% dplyr::filter(Pct > 50.0) %>% select(winner,no)

supers_2 <- 1:nrow(supers) %>% purrr::map(function(x)
  sim_score(supers$team1[x],supers$team2[x],reseed = T) %>% dplyr::mutate(no = supers$no[x]) %>%
    dplyr::group_by(winner,no) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% dplyr::filter(Pct > 50.0) %>% select(winner,no)

#filter out teams that swept
cws <- semi_join(supers_1,supers_2,by=c("winner","no"))

rem_series <- anti_join(supers_1,supers_2,by="winner") %>% pull(winner)

#run game 3 of supers if necessary
supers_adj <- supers %>% filter(team1 %in% rem_series | team2 %in% rem_series)

supers_3 <- 1:nrow(supers_adj) %>% purrr::map(function(x)
  sim_score(supers_adj$team1[x],supers_adj$team2[x],reseed = T) %>% dplyr::mutate(no = supers_adj$no[x]) %>%
    dplyr::group_by(winner,no) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% dplyr::filter(Pct > 50.0) %>% select(winner,no)

#get college world series teams
cws <- bind_rows(cws,supers_3) %>% select(winner)

#CWS Bracket THE TEAMS MAY BE DIFFERENT THAN DISPLAYED MAKE SURE YOU CORRECTLY IDENTIFY THE 8 TEAMS IN YOUR REGION AND SEED THEM CORRECTLY
bracket_1 <- data.frame(team1 = c("Arkansas","Arizona"),team1seed = c(1,2),
                        team2 = c("UC Irvine","East Carolina"),team2seed = c(4,3),
                        stringsAsFactors = F)

bracket_2 <- data.frame(team1 = c("Texas","TCU"),team1seed = c(1,2),
                        team2 = c("Central Mich.","Wright St."), team2seed = c(4,3),
                        stringsAsFactors = F)

#Run CWS Simulation
bracket_1_output <- 1:nrow(bracket_1) %>% purrr::map(function(x)
  sim_score(bracket_1$team1[x],bracket_1$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

wb_bracket_1 <- bracket_1_output %>% filter(n > 500) %>% select(winner) %>%
  mutate(team1 = c(slice(.,1),NA), team2 = c(slice(.,2),NA)) %>% slice(1) %>% select(team1,team2)

lb_bracket_1 <- bracket_1_output %>% filter(n < 501) %>% select(winner) %>%
  mutate(team1 = c(slice(.,1),NA), team2 = c(slice(.,2),NA)) %>% slice(1) %>% select(team1,team2)


wb_1_output <- 1:nrow(wb_bracket_1) %>% purrr::map(function(x)
  sim_score(wb_bracket_1$team1[x],wb_bracket_1$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

wb_1_final <- wb_1_output %>% filter(n > 500) %>% select(winner) %>% rename(team1 = winner)

lb_1_output <- 1:nrow(lb_bracket_1) %>% purrr::map(function(x)
  sim_score(lb_bracket_1$team1[x],lb_bracket_1$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

lb_bracket_1 <- lb_1_output %>% filter(n > 500) %>% select(winner) %>% rename(team1 = winner)
lb_bracket_1 <- lb_bracket_1 %>% mutate(team2 = ifelse(nrow(wb_1_output)==1,wb_bracket_1$team2,wb_1_output %>% filter(n < 501) %>% pull(winner)),
                                        team1 = as.character(team1),
                                        team2 = as.character(team2)) 

lb_final_1 <- 1:nrow(lb_bracket_1) %>% purrr::map(function(x)
  sim_score(lb_bracket_1$team1[x],lb_bracket_1$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

wb_1_final <- bind_cols(wb_1_final,lb_final_1 %>% filter(Pct > 50.0) %>% select(winner) %>% rename(team2 = winner))

bracket_1_final <- 1:nrow(wb_1_final) %>% purrr::map(function(x)
  sim_score(wb_1_final$team1[x],wb_1_final$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% filter(Pct > 50.0)

### RUN ONLY IF THE LOSER'S BRACKET TEAM WINS ###
bracket_1_final <- 1:nrow(wb_1_final) %>% purrr::map(function(x)
  sim_score(wb_1_final$team1[x],wb_1_final$team2[x], reseed = T) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% filter(Pct > 50.0)

bracket_2_output <- 1:nrow(bracket_2) %>% purrr::map(function(x)
  sim_score(bracket_2$team1[x],bracket_2$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

wb_bracket_2 <- bracket_2_output %>% filter(n > 500) %>% select(winner) %>%
  mutate(team1 = c(slice(.,1),NA), team2 = c(slice(.,2),NA)) %>% slice(1) %>% select(team1,team2)

lb_bracket_2 <- bracket_2_output %>% filter(n < 501) %>% select(winner) %>%
  mutate(team1 = c(slice(.,1),NA), team2 = c(slice(.,2),NA)) %>% slice(1) %>% select(team1,team2)


wb_2_output <- 1:nrow(wb_bracket_2) %>% purrr::map(function(x)
  sim_score(wb_bracket_2$team1[x],wb_bracket_2$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

wb_2_final <- wb_2_output %>% filter(n > 500) %>% select(winner) %>% rename(team1 = winner)

lb_2_output <- 1:nrow(lb_bracket_2) %>% purrr::map(function(x)
  sim_score(lb_bracket_2$team1[x],lb_bracket_2$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

lb_bracket_2 <- lb_2_output %>% filter(n > 500) %>% select(winner) %>% rename(team2 = winner)
lb_bracket_2 <- lb_bracket_2 %>% mutate(team1 = ifelse(nrow(wb_2_output)==1,wb_bracket_2$team2,wb_2_output %>% filter(n < 502) %>% pull(winner)),
                                        team1 = as.character(team2),
                                        team2 = as.character(team2)) 

lb_final_2 <- 1:nrow(lb_bracket_2) %>% purrr::map(function(x)
  sim_score(lb_bracket_2$team1[x],lb_bracket_2$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()

wb_2_final <- bind_cols(wb_2_final,lb_final_2 %>% filter(Pct > 50.0) %>% select(winner) %>% rename(team2 = winner))

bracket_2_final <- 1:nrow(wb_2_final) %>% purrr::map(function(x)
  sim_score(wb_2_final$team1[x],wb_2_final$team2[x]) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% filter(Pct > 50.0)

### RUN ONLY IF THE LOSER'S BRACKET TEAM WINS ###
bracket_2_final <- 1:nrow(wb_2_final) %>% purrr::map(function(x)
  sim_score(wb_2_final$team1[x],wb_2_final$team2[x], reseed = T) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows() %>% filter(Pct > 50.0)

#The Final 2 Teams
finals <- bind_cols(bracket_1_final %>% select(winner) %>% rename(team1 = winner),
                    bracket_2_final %>% select(winner) %>% rename(team2 = winner))

#Run this until a team wins twice out of 3
finals_series <- 1:nrow(finals) %>% purrr::map(function(x)
  sim_score(finals$team1[x],finals$team2[x], reseed = T) %>%
    dplyr::group_by(winner) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(Pct = n/sum(n),
                  Pct = round(100*Pct,1))) %>% bind_rows()
