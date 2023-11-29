library(nflreadr)
library(nflplotR)
library(tidyverse)

pbp <- load_participation(include_pbp = T)

pass_rush_epa <- pbp %>% filter(pass == 1) %>%
  group_by(defteam) %>% summarise(plays = n(),
                                  avg_pass_rushers = round(mean(number_of_pass_rushers,na.rm=T),3),
                                  epa = mean(epa,na.rm=T))

ggplot2::ggplot(pass_rush_epa,aes(x=avg_pass_rushers,y=epa)) +
  geom_mean_lines(aes(x0 = avg_pass_rushers, y0 = epa)) +
  geom_nfl_logos(aes(team_abbr = defteam), width = 0.055) +
  theme_classic() +
  ggtitle("EPA/play on Average Amount of Pass Rushers",
          "2023 Season Through Week 12") +
  xlab('Average Amount of Pass Rushers') +
  ylab('EPA/play') +
  labs(caption = "Data: nflreadr, Plot: @RobertFrey40")