
#VIEW THIS LINK BELOW FOR SOME AWESOMENESS
#mlb.com/video/search

#install baseballr
devtools::install_github("BillPetti/baseballr")

#call baseballr in your library
library(baseballr)

#Create a custom function to scrape every week of the 2019 season
savant_scrape = function(weekstart, weekend, type) {
  df = baseballr::scrape_statcast_savant(start_date = weekstart,
                                         end_date = weekend, player_type = type)
}

#create a list and scrape every week of the 2019 season and place it into a list
weeks = 26
savantweeks <- list()
for (i in (1:weeks)) {
  Start = as.Date('2019-03-21')
  End = as.Date('2019-03-28')
  dat = savant_scrape(Start + i*7, End + i*7, 'batter')
  dat$week = i
  savantweeks[[i]] <- dat
}

#Make one big data frame of the list of data frames
Savant = plyr::ldply(savantweeks,data.frame)

#Write the csv file, so you can use it again in the future without having to take time to run code
write.csv(Savant,"Savant.csv")

#load in tidyverse and stringr packages
library(tidyverse)
library(stringr)

#Create a sample dataset of only plays that have a description available (des)
Sample = Savant %>% filter(is.na(des)==FALSE) %>% sample_n(20)

#remove periods, commas, and parentheses in description
Sample$des = str_replace_all(Sample$des,"\\.|\\,|\\(|\\)","")
#replace sapces with dashes for purposes to convert description to a mlb video url
Sample$des = str_replace_all(Sample$des," ","-")

#paste the converted description with the rest of the url to make it complete
Sample$video = paste0("https://www.mlb.com/video/",tolower(Sample$des))

#Test some of them to see if it works :)
Sample$video[1]