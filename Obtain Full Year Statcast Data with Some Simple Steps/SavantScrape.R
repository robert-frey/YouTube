#Install devtools to be able to install from github
#install.packages('devtools')
library(devtools)
#Bill Petti's Github Page for baseballr
#http://billpetti.github.io/baseballr/
install_github("BillPetti/baseballr")

#load Statcast data week by week, since it can only load 10 days at a time or 40,000 observations
#scrape_statcast_savant scrapes data from Savant given the game dates and the player types
date328407 = baseballr::scrape_statcast_savant(start_date = '2019-03-28',
                                               end_date = '2019-04-07', player_type = 'batter')

date408414 = baseballr::scrape_statcast_savant(start_date = '2019-04-08',
                                               end_date = '2019-04-14', player_type = 'batter')

date415421 = baseballr::scrape_statcast_savant(start_date = '2019-04-15',
                                               end_date = '2019-04-21', player_type = 'batter')

date422428 = baseballr::scrape_statcast_savant(start_date = '2019-04-22',
                                               end_date = '2019-04-28', player_type = 'batter')

date429505 = baseballr::scrape_statcast_savant(start_date = '2019-04-29',
                                               end_date = '2019-05-05', player_type = 'batter')

date506512 = baseballr::scrape_statcast_savant(start_date = '2019-05-06',
                                               end_date = '2019-05-12', player_type = 'batter')

date513519 = baseballr::scrape_statcast_savant(start_date = '2019-05-13',
                                               end_date = '2019-05-19', player_type = 'batter')

date520526 = baseballr::scrape_statcast_savant(start_date = '2019-05-20',
                                               end_date = '2019-05-26', player_type = 'batter')

date527602 = baseballr::scrape_statcast_savant(start_date = '2019-05-27',
                                               end_date = '2019-06-02', player_type = 'batter')

date603609 = baseballr::scrape_statcast_savant(start_date = '2019-06-03',
                                               end_date = '2019-06-09', player_type = 'batter')

date610616 = baseballr::scrape_statcast_savant(start_date = '2019-06-10',
                                               end_date = '2019-06-16', player_type = 'batter')

date617623 = baseballr::scrape_statcast_savant(start_date = '2019-06-17',
                                               end_date = '2019-06-23', player_type = 'batter')

date624630 = baseballr::scrape_statcast_savant(start_date = '2019-06-24',
                                               end_date = '2019-06-30', player_type = 'batter')

date701707 = baseballr::scrape_statcast_savant(start_date = '2019-07-01',
                                               end_date = '2019-07-07', player_type = 'batter')

date708714 = baseballr::scrape_statcast_savant(start_date = '2019-07-08',
                                               end_date = '2019-07-14', player_type = 'batter')

date715721 = baseballr::scrape_statcast_savant(start_date = '2019-07-15',
                                               end_date = '2019-07-21', player_type = 'batter')

date722728 = baseballr::scrape_statcast_savant(start_date = '2019-07-22',
                                               end_date = '2019-07-28', player_type = 'batter')

date729804 = baseballr::scrape_statcast_savant(start_date = '2019-07-29',
                                               end_date = '2019-08-04', player_type = 'batter')

date805811 = baseballr::scrape_statcast_savant(start_date = '2019-08-05',
                                               end_date = '2019-08-11', player_type = 'batter')

date812818 = baseballr::scrape_statcast_savant(start_date = '2019-08-12',
                                               end_date = '2019-08-18', player_type = 'batter')

date819825 = baseballr::scrape_statcast_savant(start_date = '2019-08-19',
                                               end_date = '2019-08-25', player_type = 'batter')

date826901 = baseballr::scrape_statcast_savant(start_date = '2019-08-26',
                                               end_date = '2019-09-01', player_type = 'batter')

date902908 = baseballr::scrape_statcast_savant(start_date = '2019-09-02',
                                               end_date = '2019-09-08', player_type = 'batter')

date909915 = baseballr::scrape_statcast_savant(start_date = '2019-09-09',
                                               end_date = '2019-09-15', player_type = 'batter')

date916922 = baseballr::scrape_statcast_savant(start_date = '2019-09-16',
                                               end_date = '2019-09-22', player_type = 'batter')

date923929 = baseballr::scrape_statcast_savant(start_date = '2019-09-23',
                                               end_date = '2019-09-29', player_type = 'batter')

#combine all data into one data frame
SavantData19 = rbind(date328407, date408414, date415421, date422428, date429505,
                     date506512, date513519, date520526, date527602, date603609,
                     date610616, date617623, date624630, date701707, date708714,
                     date715721, date722728, date729804, date805811, date812818,
                     date819825, date826901, date902908, date909915, date916922,
                     date923929)

#Since the dataset is large, I tend to create a smaller dataset to see the data for cleaning purposes
SavantOBS = SavantData19 %>% sample_n(10)

#write file to a csv on your machine (csv is a comma separated value excel file)
write.csv(SavantData19,"SavantHittingData19.csv", row.names = F)

#read in the csv file that you just created to the dataframe
SavantData19 = read.csv("SavantHittingData19.csv", stringsAsFactors = F)

#Alternative way (quicker way) is to write the df to a local database file (ie SQLite)
#install.packages('RSQLite')
#install.packages('DBI')
library(RSQLite)
library(DBI)

#connecting a database
db = dbConnect(SQLite(), dbname ="Statcast.sqlite")

#writing the statcast table we created to the database
dbWriteTable(conn = db, name = "Statcast_Hitting", SavantData19, overwrite=T, row.names = F)

#write SQL code that selects the first five observations from the database table (in this case, the table is called "Statcast_Hitting")
dbGetQuery(conn = db, "SELECT * FROM Statcast_Hitting LIMIT 5")

#write the Query to a dataframe (again another way to write data)
SavantData19 = dbGetQuery(conn = db, "SELECT * FROM Statcast_Hitting")

#call tidyverse for dplyr purposes
library(tidyverse)
#Same type of code as line 118, but written in dplyr, slice(1:5) selects the first five rows of the dataset
SavantData19 %>% slice(1:5)

#A query that displays the top HR hitters on pitches 95+ Miles Per Hour (MPH)
SavantData19 %>%
  #we select the necessary variables, don't forget the piping operator (%>%) to signal that we want to continue our code
  select(player_name, events, launch_speed, release_speed) %>%
  #we create a filter that keeps rows that ended with a home run in the 'events' column and rows that have a pitch speed (ie release_speed) greater than 95 MPH
  filter(events == "home_run", release_speed >= 95) %>%
  #group by each player
  group_by(player_name) %>%
  #summarise the amount of home runs hit and display the average exit velocity on those home runs
  summarise(HR = n(), AvgEV = mean(launch_speed)) %>%
  #sort by highest to lowest with desx
  arrange(desc(HR))

#create a table that displays the number of ABs with an infield shift
ABs = SavantData19 %>%
  filter(events != "NA", if_fielding_alignment == "Infield shift") %>%
  select(player_name, events) %>%
  group_by(player_name) %>%
  summarise(ABs = n())

#create a table that displays the number of Hits with an infield shift
############## WATCH MY YOUTUBE VIDEO FOR THE CODE ####################
############## WATCH MY YOUTUBE VIDEO FOR THE CODE ####################
############## WATCH MY YOUTUBE VIDEO FOR THE CODE ####################

#merge both tables together and filter players with at least 100 ABs with the shift on
#create a column that displays the batting average on those events and sort desc by batting average
#display only the top 10 players in this category
############## WATCH MY YOUTUBE VIDEO FOR THE CODE ####################
############## WATCH MY YOUTUBE VIDEO FOR THE CODE ####################
############## WATCH MY YOUTUBE VIDEO FOR THE CODE ####################

