#read in file, either csv or through SQLite in my previous video
#HData = read.csv("SavantHittingData19.csv", stringsAsFactors = F)

#SQLite way
library(RSQLite)
library(DBI)
#Connect to database
db = dbConnect(SQLite(), dbname ="Statcast.sqlite")
#load in data via a SQL query
HData <- dbGetQuery(conn = db, "SELECT * FROM Statcast_Hitting")

#install these packages if you haven't already by removing the '#' in front of them
#install.packages('devtools')
#https://baseballwithr.wordpress.com/author/bayesball/
#devtools::install_github("bayesball/CalledStrike")
#devools::install_github("BillPetti/baseballr")
#install.packages('mgcv')

#load in libraries
library(mgcv)
library(tidyverse)
library(baseballr)
library(CalledStrike)

#Spray Angle Plots
LgRSA = sa_plot(HData %>% filter(stand == "R"), title = "League Spray Angle of RHH")
RSA = sa_plot(HData %>% filter(player_name == "Mike Trout"), title = "Mike Trout Spray Angle")

LgLSA = sa_plot(HData %>% filter(stand == "L"), title = "League Spray Angle of LHH")
LSA = sa_plot(HData %>% filter(player_name == "Cody Bellinger"), title = "Cody Bellinger Spray Angle")

#Exit Velocity Plots
LgLEV = ls_plot(HData %>% filter(stand == "L"), title = "League Exit Velo of LHH")
LEV = ls_plot(HData %>% filter(player_name == "Cody Bellinger"), title = "Cody Bellinger Exit Velo")

LgREV = ls_plot(HData %>% filter(stand == "R"), title = "League Exit Velo of RHH")
REV = ls_plot(HData %>% filter(player_name == "Mike Trout"), title = "Mike Trout Exit Velo")

#Launch Angle Plots
LgLLA = la_plot(HData %>% filter(stand == "L"), title = "League Launch Angle of LHH")
LLA = la_plot(HData %>% filter(player_name == "Cody Bellinger"), title = "Cody Bellinger Launch Angle")

LgRLA = la_plot(HData %>% filter(stand == "R"), title = "League Launch Angle of RHH")
RLA = la_plot(HData %>% filter(player_name == "Mike Trout"), title = "Mike Trout Launch Angle")

#Home Run Probability Plots
LgHRL = home_run_plot(HData %>% filter(stand == "L"), "League HR Prob for LHH")
HRL = home_run_plot(HData %>% filter(player_name == "Cody Bellinger"), "Cody Bellinger HR Prob")

LgHRR = home_run_plot(HData %>% filter(stand == "R"), "League HR Prob for RHH")
HRR = home_run_plot(HData %>% filter(player_name == "Mike Trout"), "Mike Trout HR Prob")


#install.packages('ggpubr')
library(ggpubr)

ggarrange(###WATCH MY YOUTUBE VIDEO FOR CODE###########################)


