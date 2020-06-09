
#load in libraries
#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages('tidyverse')
library(tidyverse)

#scrape draft data from 2009 to 2019 quickly using the map function and Bill Petti's baseballr function
draft_list <- 2009:2019 %>% purrr::map(function(x) baseballr::get_draft_mlb(x))

#convert list to a singular data frame
draft_data <- plyr::ldply(draft_list,data.frame)

#read in csvs from Github
draft_2020 <- read.csv("draft_data.csv", stringsAsFactors = F)

#data acquired from FanGraphs:
#https://www.fangraphs.com/prospects/the-board/2020-mlb-draft/summary?sort=-1,1&type=0
fg_batters <- read.csv("fg_draft_hitters.csv", stringsAsFactors = F)

fg_pitchers <- read.csv("fg_draft_pitchers.csv", stringsAsFactors = F)

#left_join first the draft data and the fangraphs hitter data
draft_2020 <- left_join(draft_2020,fg_batters,by="Name")

#same for the pitching data
draft_2020_final <- left_join(draft_2020,fg_pitchers,by="Name")

#convert column types as well as replace NAs in certain columns with values from other columns
draft_2020_final <- draft_2020_final %>% ####WATCH YOUTUBE VIDEO FOR CODE###  %>% 
  mutate(Athl.x = coalesce(Athl.x,Athl.y),
         Frame.x = coalesce(Frame.x,Frame.y),
         Perf.x = coalesce(Perf.x,Perf.y),
         FG_Rank.x = coalesce(FG_Rank.x,FG_Rank.y),
         FG_Pos = coalesce(FG_Pos,Pos),
         Age.x = coalesce(Age.x,Age.y),
         FV.x = coalesce(FV.x,FV.y)) %>%
  #select specific variables and rename variables
  select(Name:FV.x,TJ.Date:Tops) %>% rename(FG_Rank = FG_Rank.x, Age = Age.x,
                                            Athleticism = Athl.x,
                                            Frame = Frame.x,
                                            Performance = Perf.x,
                                            FV = FV.x,
                                            MLB_Rank = Rank)

#Create plot of players by class
ggplot(draft_2020_final,aes(x=reorder(Class,Class,
                                      function(x)-length(x)))) +
  geom_bar(fill="blue") +
  xlab("Class") +
  ylab("Number of Players") +
  labs(title = "Top 289 Prospects by Class Type",
       subtitle = "2020 Draft") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
