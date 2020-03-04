library(baseballr)
library(dplyr)

#Scrape each season of Kershaw's pitches since the Statcast Era (2015)
Kersh15 = scrape_statcast_savant(start_date = "2015-01-01", end_date = "2015-11-01", playerid = 477132, player_type = "pitcher")
Kersh16 = scrape_statcast_savant(start_date = "2016-01-01", end_date = "2016-11-01", playerid = 477132, player_type = "pitcher")
Kersh17 = scrape_statcast_savant(start_date = "2017-01-01", end_date = "2017-11-01", playerid = 477132, player_type = "pitcher")
Kersh18 = scrape_statcast_savant(start_date = "2018-01-01", end_date = "2018-11-01", playerid = 477132, player_type = "pitcher")
Kersh19 = scrape_statcast_savant(start_date = "2019-01-01", end_date = "2019-11-01", playerid = 477132, player_type = "pitcher")

#Combine the statcast data into one data frame
Kersh = rbind(Kersh15,Kersh16,Kersh17,Kersh18,Kersh19)

#Filter by only his breaking balls SL- Slider, CU - Curveball
Kersh = Kersh %>% filter(pitch_type == "SL" | pitch_type == "CU")

#Count Swings
Kersh = Kersh %>% mutate(swing = ifelse(description == "foul_tip" | description == "hit_into_play" |
                           description == "hit_into_play_no_out" | description == "swinging_strike_blocked" |
                           description == "foul" | description == "hit_into_play_score" |
                           description == "swinging_strike",1,0),
                         #Count Misses
                         miss = ifelse(description=="swinging_strike",1,
                                       ifelse(description=="swinging_strike_blocked",1,
                                              ifelse(description=="foul_tip",1,0))),
                         game_date = as.Date(game_date))

#Whiff Rate on Swings
Kersh %>% dplyr::summarise(Whiff_Rate = sum(miss)/sum(swing),
                           Swing_Rate = sum(swing)/nrow(Kersh))

#Filter by Game 5
Gm5 = Kersh %>% filter(game_date == "2017-10-29")

#Kershaw's page
#https://www.baseball-reference.com/players/k/kershcl01-pitch.shtml
sum(Kersh$swing)/(33+21+27+26+29+2+3+2+1+2+3+1+3+2+2)
sum(Kersh$miss)/(33+21+27+26+29+2+3+2+1+2+3+1+3+2+2)

#Postseason data only
PostKersh15 = scrape_statcast_savant(start_date = "2015-10-06", end_date = "2015-11-01", playerid = 477132, player_type = "pitcher")
PostKersh16 = scrape_statcast_savant(start_date = "2016-10-04", end_date = "2016-11-01", playerid = 477132, player_type = "pitcher")
PostKersh17 = scrape_statcast_savant(start_date = "2017-10-03", end_date = "2017-11-01", playerid = 477132, player_type = "pitcher")
PostKersh18 = scrape_statcast_savant(start_date = "2018-10-02", end_date = "2018-11-01", playerid = 477132, player_type = "pitcher")
PostKersh19 = scrape_statcast_savant(start_date = "2019-10-01", end_date = "2019-11-01", playerid = 477132, player_type = "pitcher")

#Combine data into a single dataframe
PostKersh = rbind(PostKersh15,PostKersh16,PostKersh17,PostKersh18,PostKersh19)

#Filter by only his breaking balls SL- Slider, CU - Curveball
PostKersh = PostKersh %>% filter(pitch_type == "SL" | pitch_type == "CU")

#Count Swings
PostKersh = PostKersh %>% mutate(swing = ifelse(description == "foul_tip" | description == "hit_into_play" |
                                          description == "hit_into_play_no_out" | description == "swinging_strike_blocked" |
                                          description == "foul" | description == "hit_into_play_score" |
                                          description == "swinging_strike",1,0),
                         #Count Misses
                         miss = ifelse(description=="swinging_strike",1,
                                       ifelse(description=="swinging_strike_blocked",1,
                                              ifelse(description=="foul_tip",1,0))),
                         game_date = as.Date(game_date))

#Whiff Rate on Swings
PostKersh %>% dplyr::summarise(Whiff_Rate = sum(miss)/sum(swing),
                           Swing_Rate = sum(swing)/nrow(PostKersh))

#Swings and Misses in Postseason
sum(PostKersh$swing)/(2+3+2+1+2+3+1+3+2+2)
sum(PostKersh$miss)/(2+3+2+1+2+3+1+3+2+2)