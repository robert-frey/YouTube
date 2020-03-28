library(baseballr)
library(tidyverse)
#Load in Statcast Data Set
Savant = read.csv("Savant.csv", stringsAsFactors = F)

#Random Sample
Sample = Savant %>% sample_n(20)

#Park Factors, per Fangraphs
PF = fg_park(2019)

#Get Team Abbreviations
ABBR = unique(Savant$home_team)

#Create data frame and merge
DF = data.frame(home_team = c("Angels","Orioles","Braves","Mets","Athletics","Indians",
                                "Tigers","Pirates","Nationals","Royals","Marlins","Rays",
                                "Dodgers","Blue Jays","Yankees","Padres","Rangers","Reds",
                                "Mariners","Phillies","Brewers","Twins","Cardinals","Red Sox",
                                "Diamondbacks","Cubs","Giants","Astros","White Sox","Rockies"),
                  abbr = ABBR)

#Merge data frames to have abbreviation into Statcast
PF = merge(x=PF,y=DF,by="home_team")
PF = PF %>% select(abbr, basic_5yr) %>% rename(home_team = abbr)

#Add 5 year park factors into dataset
Savant = merge(x=Savant,y=PF,by="home_team",all.x = T)

#League-wide statistics by contact
PF = statline_from_statcast(Savant %>% filter(basic_5yr < 100),base = "contact")
PF
HF = statline_from_statcast(Savant %>% filter(basic_5yr > 100),base = "contact")
HF
NEU = statline_from_statcast(Savant %>% filter(basic_5yr == 100),base = "contact")
NEU

#Subset Data by Specific Player by creating custom function
get_statline_by_env = function(df, name, environment = "neutral") {
dfa = df %>% filter(player_name == name)
hf = dfa %>% filter(basic_5yr > 100)
pf = dfa %>% filter(basic_5yr < 100)
neu = dfa %>% filter(basic_5yr == 100)

if (environment == "neutral") {
#####WATCH MY YOUTUBE VIDEO FOR CODE#####
} else if (environment == "hitter") {
  #####WATCH MY YOUTUBE VIDEO FOR CODE#####
} else if (environment == "pitcher") {
  #####WATCH MY YOUTUBE VIDEO FOR CODE#####
}
return(sline)
}

#Test the function
get_statline_by_env(Savant,"Yasmani Grandal",environment = "pitcher")

#add environment column, then plot
Tot$environment = c("Hitter","Pitcher","Neutral")

ggplot(Tot, aes(x=environment, y=woba, fill = batted_balls)) +
  geom_col() +
  ggtitle("Cody Bellinger wOBAcon by environment", subtitle = "Colored by Batted Balls")