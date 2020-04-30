library(baseballr)
library(tidyverse)

#lookup school, Vanderbilt is this case for the video
Vandy = school_id_lu("Vanderbilt")

#read in the csv file that is attached that contains linear weights
ncaa_guts = read.csv("ncaa_guts.csv", stringsAsFactors = F)

#select necessary variables and rename woba to lgwoba to avoid confusion
ncaa_guts = ncaa_guts %>% select(year,division,wOBA,wBB,wHBP,w1B,w2B,w3B,wHR) %>% rename(lgwOBA = wOBA)

#Use the edit function to edit the 2020 season to 2019
Vandy = edit(Vandy)

#create a list and loop through the years of 2013-2019 to get hitting data by player at Vandy
VandyHit <- list()
for (i in 1:nrow(Vandy)) {
  VandyHit[[i]] <- ncaa_scrape(teamid = Vandy$school_id[i],year = Vandy$year[i], type = "batting")
}

#convert list of dataframes to a singular data frame
Vanderbilt = plyr::ldply(VandyHit,data.frame)

#Ensure all rows of school, conference, and division are properly names, as well as removed any observations with no player_id
Vanderbilt = Vanderbilt %>% mutate(school = "Vanderbilt", conference = "SEC", division = 1) %>% filter(is.na(player_id)==FALSE)

#create a custom function to obtain a player's IBB totals by year
get_IBB <- function(player_id, year) {
  #subsets the data by the unique year id
  year_id <- subset(ncaa_season_id_lu, season == year, select = id)
  #subsets the data by the unique batting id
  batting_id <- subset(ncaa_season_id_lu, season == year, select = batting_id)
  #obtains a url for the batter to then read the data table of player's totals by year
  batting_url <- paste0("https://stats.ncaa.org/player/index?id=", year_id,"&stats_player_seq=", player_id,"&year_stat_category_id=", batting_id)
  #reads the data table from the url
  batting_payload <- xml2::read_html(batting_url)
    #data cleaning after extracting table  
      payload_df <- batting_payload %>%
        rvest::html_nodes('table') %>%
        .[3] %>%
        rvest::html_table(fill = T) %>%
        as.data.frame() %>%
        .[-1,]
      
      names(payload_df) <- payload_df[1,]
      
      payload_df <- payload_df[-1,]
      
      year_df = data.frame(#WATCH YOUTUBE VIDEO FOR CODE)
      )
      #join data frame and year data frame to filter by year and pull the IBBs
      payload_df <- left_join(payload_df,year_df,by="Year")
      ibb <- payload_df %>% filter(Season == {{year}}) %>% pull(IBB)

      
    
  
  return(ibb)
  
}
#loop through each batter and pull IBB data (might take some time)
for (i in 1:nrow(Vanderbilt)) {
  svMisc::progress(i,nrow(Vanderbilt))
  Vanderbilt$IBB[[i]] <- get_IBB(Vanderbilt$player_id[i],Vanderbilt$year[i])
}

#Data cleaning, converting variables to numeric, replacing NAs with 0, adding the columns single and plate appearances
Vanderbilt = Vanderbilt %>% mutate_at(vars(AB,H,X2B,X3B,HR,BB,HBP,SF,SH,IBB),as.numeric) %>% replace_na(list(H=0,X2B=0,X3B=0,HR=0,IBB=0,SF=0,SH=0,BB=0,HBP=0)) %>% mutate(X1B =(H-X2B-X3B-HR),
                                                                                                                  PA = AB+H+BB+HBP+SF+SH)

#left join the ncaa_guts page with the data frame.
Vanderbilt = left_join(Vanderbilt,ncaa_guts,by=c("division","year"))

#create a function called get_woba that calculates each player's wOBA accurately by year
get_woba <- function(df) {
colnames <- c("wBB","BB","wHBP","HBP","w1B","X1B","w2B","X2B","w3B","X3B","wHR","HR","PA","IBB")
if (!all(colnames %in% names(df))) warning("You must have the following variables in your dataset to calculate woBA: 'wBB','BB','wHBP','HBP','w1B','X1B','w2B','X2B','w3B','X3B','wHR','HR','PA','IBB'")
df$wOBA <- round((((df$wBB * df$BB) + (df$wHBP * df$HBP) + (df$w1B * df$X1B) + (df$w2B * df$X2B) + 	(df$w3B * df$X3B) + (df$wHR * df$HR))/(df$PA-df$IBB)),3)
return(df)
}

#run the get wOBA function
Vanderbilt = get_woba(Vanderbilt)

#display the Top 5 hitters at Vandy from 2013-2019 with 200 PAs or more by wOBA
head(Vanderbilt %>%  filter(PA >= 200) %>% arrange(desc(wOBA)))
