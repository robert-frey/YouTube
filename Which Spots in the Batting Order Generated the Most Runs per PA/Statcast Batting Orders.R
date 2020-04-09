#Load in libraries
library(baseballr)
library(tidyverse)

#Load in Dataset
Savant = read.csv("SavantHittingData19.csv", stringsAsFactors = F)

#Select Unique games from Statcast Data
game_ids = data.frame(game_pk = unique(Savant$game_pk))

#Loop to getting batting orders/subs/etc for each game, then add a mutate call to add the game_pk
orders <- list()
for (i in 1:nrow(game_ids)) {
  orders[[i]] = get_batting_orders(game_ids$game_pk[i], type = "all") %>% select(id,abbreviation,batting_order) %>% mutate(game_pk = game_ids$game_pk[i])
}

#convert the list of data frames to a singular data frame
battingorders = plyr::ldply(orders, data.frame)

#rename id to batter to merge into Statcast Data and remove unnecessary columns
battingorders = battingorders %>% rename(batter = id) %>% select(batter,abbreviation,batting_order,game_pk)

#Subset the data by plate appearance, and add run expectancy data
Savant = run_expectancy_code(Savant, level = "plate appearance")

#Merge the dataset by both batter id and game id 
Savant = merge(Savant,battingorders,by = c("batter","game_pk"), all.x = T)

#Arrange the Dataset so that it's in order from earliest game, then by inning, at bat, and pitch
Savant = Savant %>% arrange(game_pk,inning, at_bat_number, pitch_number) 

#convert batting order to numeric in case when need to manipulate
Savant$batting_order = as.numeric(Savant$batting_order)

#check for NAs in batting order
sun(is.na(Savant$batting_order))

#No NAs, so we can convert the batting order to a factor
Savant$batting_order = as.factor(Savant$batting_order)

#Add columns for grouping and calculation purposes
Savant = Savant %>% mutate(runs_scored_on_pitch = coalesce(runs_scored_on_pitch, 0),
                           batting = ifelse(inning_topbot == "Top",away_team,home_team),
                           fielding = ifelse(inning_topbot == "Top",home_team,away_team))

#Load in data from csv file that is attached in the folder, which is the League data for teams (ie AL or NL)
Teams = read.csv("Team_Leagues.csv",stringsAsFactors = F)

#Subset data by home and away teams
Home = Teams %>% select(home_team, home_lg)
Away = Teams %>% select(away_team, away_lg)

#Left join the Home and Away Datasets to our Savant Dataset
Savant = left_join(Savant,Home,by="home_team")
Savant = left_join(Savant,Away,by="away_team")

#Get Plate Appearance info by batting order, first one is across MLB, as well as other metrics like Run Expectancy24, runs driven in per pa, wOBAcon, etc.
#4858 is the amount of ML games in the 2019 Season
Savant %>% 
  #WATCH YOUTUBE VIDEO FOR CODE#
  summarise(pa_per_game = #WATCH YOUTUBE VIDEO FOR CODE#
              , avg_re24 = mean(re24, na.rm = T), runs_driven_in_per_pa = sum(runs_scored_on_pitch)/#WATCH YOUTUBE VIDEO FOR CODE#,
            wobacon = mean(estimated_woba_using_speedangle, na.rm = T), avg_ev = mean(launch_speed, na.rm = T))

#Subset data by American League Games
AL = Savant %>% filter(home_lg == "AL")

#Same as above, but for American League games
#2429 is half the amount of ML games in 2019
AL %>%
  #WATCH YOUTUBE VIDEO FOR CODE# %>%
    summarise(pa_per_game = #WATCH YOUTUBE VIDEO FOR CODE#
                , avg_re24 = mean(re24, na.rm = T), runs_driven_in_per_pa = sum(runs_scored_on_pitch)/#WATCH YOUTUBE VIDEO FOR CODE#,
              wobacon = mean(estimated_woba_using_speedangle, na.rm = T), avg_ev = mean(launch_speed, na.rm = T))
 
#Subset data by National League Games
NL = Savant %>% filter(home_lg == "NL")

#Same as above, but for National League games
#2429 is half the amount of ML games in 2019

NL %>%
  #WATCH YOUTUBE VIDEO FOR CODE# %>%
  summarise(pa_per_game = #WATCH YOUTUBE VIDEO FOR CODE#
              , avg_re24 = mean(re24, na.rm = T), runs_driven_in_per_pa = sum(runs_scored_on_pitch)/#WATCH YOUTUBE VIDEO FOR CODE#,
            wobacon = mean(estimated_woba_using_speedangle, na.rm = T), avg_ev = mean(launch_speed, na.rm = T))

#Team Specific data, in this case, the Angels, great to compare how valuable their spot in the batting order is compared to the league average
#I used Mike Trout as a comparison to the league, and how much more value he creates in the 2 hole compared to the league average 2 hole hitter
LAA = Savant %>% filter(batting == "LAA")

LAA %>% 
  #WATCH YOUTUBE VIDEO FOR CODE# %>%
  summarise(pa_per_game = #WATCH YOUTUBE VIDEO FOR CODE#
              , avg_re24 = mean(re24, na.rm = T), runs_driven_in_per_pa = sum(runs_scored_on_pitch)/#WATCH YOUTUBE VIDEO FOR CODE#,
            wobacon = mean(estimated_woba_using_speedangle, na.rm = T), avg_ev = mean(launch_speed, na.rm = T))

