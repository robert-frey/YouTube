#Install these Packages if you haven't already
#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages('stringr')
library(stringr)
#install.packages('tidyverse')
library(tidyverse)

#School ID lookup, use any team you'd like
school_id_lu(school_name = "UIC")

#Get the team's schedule info in the year you'd like
#we saw from the previous code that UIC's ID is 302
UIC <- get_ncaa_schedule_info(teamid = 302, year = 2019)

#Get the pbp data of each game that UIC played in 2019
UICpbp <- list()
for (i in 1:nrow(UIC)) {
  UICpbp[[i]] <- get_ncaa_baseball_pbp(UIC$game_info_url[i])
}

#combine the list of data frames into one data frame
UIC19 = plyr::ldply(UICpbp,data.frame)

#Using stringr
#we extract the batter name using the word function, sometimes batter name is last name only
UIC19$batter <- word(string = UIC19$description, start = 1, end = 2)
#remove unnecessary strings in the batter column
UIC19$batter <- str_remove(UIC19$batter,"struck|singled|reached|fouled|flied|grounded|
                           homered|hit|walked.|walked|advanced|doubled|popped|foul|out|tripled|lined|
                           intentionally|singled,|infield|walked3a|homered,")
#remove unncessary leading and trailing strings in the batter column
UIC19$batter <- str_squish(UIC19$batter)

#if the string detected is a slash or to p for, the corresponding batter row should be NA
UIC19$batter <- ifelse(str_detect(UIC19$description, "to p for")==TRUE,NA,UIC19$batter)
UIC19$batter <- ifelse(str_detect(UIC19$description, "\\/")==TRUE,NA,UIC19$batter)


#Extracting some (not all) action strings
#####WATCH MY YOUTUBE VIDEO FOR CODE######

#Extracting batted ball locations
#####WATCH MY YOUTUBE VIDEO FOR CODE######