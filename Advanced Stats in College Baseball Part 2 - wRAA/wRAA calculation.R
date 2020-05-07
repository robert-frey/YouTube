library(baseballr)
library(tidyverse)

#read in sample csv file, which will be attached in GitHub
Batters = read.csv("SampleBatters.csv", stringsAsFactors = F)

#read in the csv file that is attached that contains linear weights
ncaa_guts = read.csv("ncaa_guts.csv", stringsAsFactors = F)

#select necessary variables and rename woba to lgwoba to avoid confusion
ncaa_guts = ncaa_guts %>% select(year,division,wOBA,wOBA_Scale,wBB,wHBP,w1B,w2B,w3B,wHR,runsWin) %>% rename(lgwOBA = wOBA)

#join the guts page with the batters data frame to have the necessary columns to calculate wRAA
Batters = left_join(Batters,ncaa_guts,by="year")

#write a function that gets the wRAA for each observation
get_wRAA <- function(###WATCH YOUTUBE VIDEO FOR CODE###) {
  wRAA = round(((wOBA-lgwOBA)/Scale) * PA,1)
}

Batters = Batters %>% mutate(wRAA = get_wRAA(###WATCH YOUTUBE VIDEO FOR CODE###
                                             ###))

#Display the top 10 players from 2013-2019 in wRAA
Batters %>% arrange(-wRAA) %>% select(year,Player,school,wRAA) %>% top_n(10)

#acquire ncaa color data thanks to ncaahoopR
devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)
colors = ncaa_colors %>% rename(school = ncaa_name)

#left join to get colors in data frame
Batters = left_join(Batters,colors,by="school")

#Create a data frame and adjust the missing/incorrect colors/urls
Top10 = Batters %>% mutate(Player_Year = paste0(Player,"-",year)) %>% arrange(-wRAA) %>% select(year,Player_Year,school,primary_color,logo_url,wRAA) %>% top_n(10) %>%
  replace_na(list(primary_color="#A91A2D", logo_url = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Dallas_Baptist_Patriots_wordmark.svg/320px-Dallas_Baptist_Patriots_wordmark.svg.png"))

Top10$logo_url = ifelse(Top10$school == "San Diego","https://upload.wikimedia.org/wikipedia/commons/thumb/7/7e/San_Diego_Toreros_logo.svg/150px-San_Diego_Toreros_logo.svg.png",Top10$logo_url)

#create a plot
#install.packages('ggimage')
library(ggimage)
#the reorder function orders the bar chart from greatest to smallest
ggplot(data = Top10, aes(x=reorder(Player_Year, -wRAA),y=wRAA)) +
  #geom_col is our bar chart, theme(legend.position = "none" means we want to remove our legend
  geom_col(fill = Top10$primary_color) + theme(legend.position = "none") + 
  #render our image, and the nudge_y allows us to place the image inside the bar
  geom_image(image = Top10$logo_url, asp = 16/9, nudge_y = -10) + 
  #labels
  xlab("Player in Year") + ylab("wRAA") +
  #display wRAA number and adjust it slightly above the bar with the vjust function
  geom_text(aes(label=wRAA), vjust = -.2, fontface = "bold") + 
  #Title
  ggtitle("Top 10 wRAA from 2013-2019 in D1 Baseball")

#Calculate Non_adjusted batting runs, a function to calculate Wins Above Replacement or WAR
Batters = Batters %>% mutate(NonAdj_BattingRuns = round(wRAA/runsWin,1))

#View the Top 10 players in Non-Adjusted Batting Runs
Batters %>% arrange(-NonAdj_BattingRuns) %>% select(year,Player,school,NonAdj_BattingRuns) %>% top_n(10)

