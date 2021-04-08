library(tidyverse)
d1_team_batting <- d1_batting %>% filter(str_detect(Player,"(Totals)"))
d1_team_batting <- d1_team_batting %>% filter(Player != "Opponent Totals")

d1_team_pitching <- d1_pitching %>% filter(str_detect(Player,"(Totals)"))
d1_team_pitching <- d1_team_pitching %>% filter(Player != "Opponent Totals")


d1_batting <- d1_batting %>% filter(str_detect(Player,"(Totals)")==F)

#Alternative way
d1_batting <- d1_batting %>% filter(Player != "Totals" | Player != "Opponent Totals")

d1_pitching <- d1_pitching %>% filter(str_detect(Player,"(Totals)")==F)


sapply(d1_batting,class)

sapply(d1_pitching,class)

d1_batting <- d1_batting %>% mutate_if(is.numeric, replace_na, replace = 0)

d1_batting <- d1_batting %>% mutate(PA = AB+BB+HBP+SF+SH) %>% filter(PA > 0)

d1_pitching <- d1_pitching %>% mutate_if(is.numeric, replace_na, replace = 0) %>% filter(BF > 0)

#install.packages('DBI')
#install.packages("RSQLite")
library(DBI)
library(RSQLite)

d1_db <- dbConnect(SQLite(),"division_1_db.sqlite")

##### WATCH YOUTUBE VIDEO FOR CODE #####

dbDisconnect(d1_db)

d1_db <- dbConnect(SQLite(),"division_1_db.sqlite")

D1BATTING <- dbGetQuery(d1_db,"SELECT * FROM D1_Batting")
D1PITCHING <- dbGetQuery(d1_db,"SELECT * FROM D1_Pitching")
D1TEAMBATTING <- dbGetQuery(d1_db,"SELECT * FROM D1_Team_Batting")
D1TEAMPITCHING <- dbGetQuery(d1_db,"SELECT * FROM D1_Team_Pitching")

dbDisconnect(d1_db)
