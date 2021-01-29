library(tidyverse)

#link: https://drive.google.com/file/d/16hmt1H6LDL_yUVckWHoHMgSMvT8omHkt/view?usp=sharing
sc_2020 <- readr::read_csv("statcast_data_2020.csv")

#link: https://app.box.com/shared/static/3ma89wn0g9tdio5gowtz01uykfqmm1ly.csv
#Thanks to @BillPetti for the csv file
spin_direction <- readr::read_csv("spin_direction_pbp.csv")

sc_2020 <- left_join(sc_2020,spin_direction,by=c("game_pk","batter","pitcher","pitch_number","inning"))

colnames(sc_2020)

# filter 4-Seam Fastball and get mean velo and spin direction of pitchers, then classify and filter guys who
# threw at least 100 fastballs
pitchers <- sc_2020 %>% dplyr::filter(pitch_name == "4-Seam Fastball") %>%
  dplyr::group_by(pitcher_name) %>% dplyr::summarise(n_pitches = n(),
                                                     mean_fb_velo = mean(release_speed, na.rm=T),
                                                     mean_spin_dir = mean(release_spin_direction,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(#### WATCH YOUTUBE VIDEO FOR CODE ####,
                mean_fb_velo = round(mean_fb_velo,1),
                mean_spin_dir = round(mean_spin_dir,1)) %>%
  dplyr::filter(n_pitches >= 100)

#Plot a boxplot of each throwing type and their spin direction
ggplot(pitchers,aes(x=type,y=mean_spin_dir,fill=type)) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  theme_classic() +
  ggtitle("Mean Spin Direction by Throwing Type")

# Null Hypothesis - the mean spin direction of Hard Throwers and Non Hard Throwers are equal
#ALt Hypothesis - the mean spin direction of each type is not equal

#95% CI 

t.test(mean_spin_dir~type, data=pitchers)