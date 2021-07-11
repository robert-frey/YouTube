library(baseballr)
library(tidyverse)

#read in csv files
draft_2021_hitting <- readr::read_csv("https://raw.githubusercontent.com/robert-frey/YouTube/master/Build%20your%20own%20Draft%20DB%20and%20Acquire%202021%20MLB%20Prospect%20Data/2021_draft_hitting_data.csv")
draft_2021_pitching <- readr::read_csv("https://raw.githubusercontent.com/robert-frey/YouTube/master/Build%20your%20own%20Draft%20DB%20and%20Acquire%202021%20MLB%20Prospect%20Data/2021_draft_pitching_data.csv")
draft_2021_summary <- readr::read_csv("https://raw.githubusercontent.com/robert-frey/YouTube/master/Build%20your%20own%20Draft%20DB%20and%20Acquire%202021%20MLB%20Prospect%20Data/2021_draft_summary_data.csv")

#join draft data together
draft_2021 <- left_join(draft_2021_summary,draft_2021_hitting,by=c("name","rank"))
draft_2021 <- left_join(draft_2021,draft_2021_pitching,by=c("name","rank"))

#look at the draft url
draft_2021$video[3]

#map the draft years
mlb_draft_data <- 1965:2020 %>% purrr::map(function(x) get_draft_mlb(x))

#coerce the draft years into a single table/data frame
mlb_draft <- bind_rows(mlb_draft_data) %>% select(-person_xref_ids)

#write the draft csv
write_csv(mlb_draft,"mlb_draft19652020.csv")

#read it in to test it out
test <- readr::read_csv("mlb_draft19652020.csv")


#which players have plus future raw power
draft_2021 %>% filter(future_raw_power >= 65) %>% arrange(-future_raw_power) %>%
  ggplot(aes(x=name,y=future_raw_power)) +
  geom_col(aes(fill = player_type)) +
  theme_classic()

#which players top out at 97 or greater
draft_2021 %>% filter(tops >= 97) %>% arrange(-tops) %>%
  ggplot(aes(x=name,y=tops)) +
  geom_col(aes(fill = player_type)) +
  theme_classic()

#how many players are high, medium, and low risk
draft_2021 %>% group_by(risk) %>% summarise(risk_totals = n()) %>%
  ggplot(aes(x=risk,y=risk_totals)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = risk_totals), nudge_y = 1)
  theme_classic()
