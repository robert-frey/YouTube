#read in csv file or sqlite database
HData = read.csv("SavantHittingData19.csv", stringsAsFactors = F)

library(RSQLite)
library(DBI)
#connecting a database
db = dbConnect(SQLite(), dbname ="Statcast.sqlite")

#write the Query to a dataframe (again another way to write data)
HData = dbGetQuery(conn = db, "SELECT * FROM Statcast_Hitting")


#install.packages('tidyverse')
library(tidyverse)

#Create a sample Dataset
Sample = HData %>% sample_n(20)

#Writing a data frame to match the arguments of the geom_spraycharts in GeomMLBStadiums
TeamCodes = data.frame(
  home_team = c("LAA","HOU","OAK","TOR","ATL","MIL","STL","CHC",
                  "ARI","LAD","SF","CLE","SEA","MIA","NYM","WSH",
                  "BAL","SD","PHI","PIT","TEX","TB","BOS","CIN",
                  "COL","KC","DET","MIN","CWS","NYY"),
  venue = c("angels","astros","athletics","blue_jays","braves",
                "brewers","cardinals","cubs","diamondbacks","dodgers",
                "giants","indians","mariners","marlins","mets","nationals","orioles",
                "padres","phillies","pirates","rangers","rays","red_sox","reds",
                "rockies","royals","tigers","twins","white_sox","yankees"),
  stringsAsFactors = F
)

#Filter only balls in Play
BIP = HData %>% filter(type == "X")
#Merge the Statcast Data with TeamCodes
BIP <- merge(BIP,TeamCodes,by="home_team",all.x = T)

#call GeomMLBStadiums
#Ben Dilday's GeomMLBStadiums
#https://github.com/bdilday/GeomMLBStadiums
#devtools::install_github("bdilday/GeomMLBStadiums")
library(GeomMLBStadiums)


#_________ converts the hitting coordinates to match the geom_spraychart plot
A = BIP %>% filter(player_name == "Mookie Betts") %>% #####WATCH MY YOUTUBE VIDEO FOR CODE#### 
  ggplot(aes(x=hc_x_, y=hc_y_, color=bb_type)) + 
  geom_spraychart(stadium_ids = "red_sox",
                  stadium_transform_coords = TRUE, 
                  stadium_segments = "all") + 
  theme_void() + 
  coord_fixed() +
  ggtitle("Mookie at Fenway")

B = BIP %>% filter(player_name == "Mookie Betts") %>% #####WATCH MY YOUTUBE VIDEO FOR CODE#### 
  ggplot(aes(x=hc_x_, y=hc_y_, color=bb_type)) + 
  geom_spraychart(stadium_ids = "dodgers",
                  stadium_transform_coords = TRUE, 
                  stadium_segments = "all") + 
  theme_void() + 
  coord_fixed() +
  ggtitle("Mookie at Dodger Stadium")

C = BIP %>% filter(player_name == "Mookie Betts") %>% #####WATCH MY YOUTUBE VIDEO FOR CODE#### 
  ggplot(aes(x=hc_x_, y=hc_y_, color=bb_type)) + 
  geom_spraychart(stadium_ids = "dodgers",
                  stadium_transform_coords = TRUE, 
                  stadium_segments = "all") + 
  theme_void() + 
  coord_fixed() +
  ggtitle("Mookie at Petco Park")

#load in patchwork to combine plots on one page
#install.packages('patchwork')
library(patchwork)

(A | C) / B

#another way to load in a specific player, through Called Strike Package
#devtools::install_github('bayesball/CalledStrike')
library(baseballr)
Trout = #####WATCH MY YOUTUBE VIDEO FOR CODE####

Trout = plyr::ldply(Trout, data.frame)

Trout = Trout %>% filter(type=="X")

Trout %>% #####WATCH MY YOUTUBE VIDEO FOR CODE####
  ggplot(aes(x=hc_x_, y=hc_y_,color=bb_type)) +
  geom_spraychart(stadium_ids = "angels",
                  stadium_transform_coords = TRUE,
                  stadium_segments = "all") +
  theme_void() +
  coord_fixed() +
  ggtitle("Trout at Angel Stadium")

Trout %>% #####WATCH MY YOUTUBE VIDEO FOR CODE####
  ggplot(aes(x=hc_x_, y=hc_y_,color=bb_type)) +
  geom_spraychart(stadium_ids = "dodgers",
                  stadium_transform_coords = TRUE,
                  stadium_segments = "all") +
  theme_void() +
  coord_fixed() +
  ggtitle("Trout at Dodger Stadium")



