#libraries
library(baseballr)
library(tidyverse)

#Read in Dataset, or whatever your Savant Dataset name is
Savant = read.csv("SavantHittingData19.csv", stringsAsFactors = F)

#Get unique game_pks (game ids)
game_pks = unique(Savant$game_pk)

#Sample Dataset
Sample = Savant %>% sample_n(10)

#Loop through game_pks to get weather info and other data, like venue_name, start time
###WATCH YOUTUBE VIDEO FOR CODE###
for (i in 1:length(game_pks)) {
 #WATCH YOUTUBE VIDEO FOR CODE###
}

#Convert list of dataframes to singular data.frame by using plyr::ldply
wdata = plyr::ldply(weather,data.frame)

#Merge statcast data with weather data we just pulled
Savant = merge(Savant,wdata,by="game_pk",all.x = T)

#write a new csv file containing the Statcast Data set
#You can either overwrite your current csv file, or create another csv file
write.csv(Savant,"Savant_with_Weather.csv", row.names = F)

#Create Temperature Groupings
###WATCH YOUTUBE VIDEO FOR CODE###

#Create some tables/plots
#Get average EV by temp and amount of BIP
Temp = Savant %>% group_by(temperature) %>% summarise(mean_EV = mean(launch_speed, na.rm = T),
                                                      results = n())

#Plot it, with size increasing by the amount of balls in play
ggplot(Temp,aes(x=temperature,y=mean_EV, group=1, size = results)) +
  geom_line(color="red")+
  geom_point(color="blue")
