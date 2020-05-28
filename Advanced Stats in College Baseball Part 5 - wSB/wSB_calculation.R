#wSB calculation
#load in library and csv files that are provided
library(tidyverse)
dat = read.csv("player_data.csv",stringsAsFactors = F)

guts = read.csv("D1_guts_wSB.csv", stringsAsFactors = F)

#join the two csv files together
dat = left_join(dat,guts,by="year")

#wSB calculation
dat = dat %>% mutate(wSB = ((SB*runSB)+(CS*runCS))-(lgwSB*(#WATCH YOUTUBE VIDEO FOR CODE#))) %>% arrange(-wSB)

#rounding
dat$wSB = round(dat$wSB,2)

#acquire ncaa color data thanks to ncaahoopR
devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)
colors = ncaa_colors
colors = colors %>% dplyr::rename(school = ncaa_name)


#left join to get colors in data frame
dat = left_join(dat,colors,by="school")

#Create a data frame and adjust the missing/incorrect colors/urls
Top10 = dat %>% mutate(Player_Year = paste0(Player,"-",year)) %>% arrange(-wSB) %>% select(year,Player_Year,school,secondary_color,logo_url,wSB) %>% top_n(10) 

  library(ggimage)
#the reorder function orders the bar chart from greatest to smallest
ggplot(data = Top10, aes(x=reorder(Player_Year, -wSB),y=wSB)) +
  #geom_col is our bar chart, theme(legend.position = "none" means we want to remove our legend
  geom_col(fill = Top10$secondary_color) + theme(legend.position = "none") + 
  #render our image, and the nudge_y allows us to place the image inside the bar
  geom_image(image = Top10$logo_url, asp = 16/9, nudge_y = -1) + 
  #labels
  xlab("Player in Year") + ylab("wSB") +
  #display wRAA number and adjust it slightly above the bar with the vjust function
  geom_text(aes(label=wSB), vjust = -.2, fontface = "bold") + 
  #Title
  ggtitle("Top 10 wSB from 2013-2019 in D1 Baseball")
