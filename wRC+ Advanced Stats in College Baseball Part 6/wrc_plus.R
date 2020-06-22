library(baseballr)
library(tidyverse)
library(rvest)

#Read CSV file
guts = read.csv("ncaa_guts_d1.csv", stringsAsFactors = F) %>% rename(lgwOBA = wOBA) %>% mutate_at(vars(year), as.character)

#Lookup School IDs
school_id_lu("Georgia Tech")

#### get hitting data of 4 selected schools ####
schools = c(457,234,147,255) %>% map(function(x) ncaa_scrape(x,2020))

ACC = bind_rows(schools)

####read in CSV file containing intentional walk data, useful for calculating wOBA####
IBB = read.csv("IBB_Data.csv", stringsAsFactors = F)

ACC = left_join(ACC,IBB,by=c("Player","school","conference"))

####Filter unnecessary players and replace NAs with 0, add PAs####
ACC = ACC %>% filter(AB > 0, GP > 0) %>% ####WATCH YOUTUBE VIDEO FOR CODE#####

####join the guts data to compute wOBA and get Park Effects for schools####
ACC = left_join(ACC,guts,by=c("year","division"))

pfs = c(457,234,147,255) %>% map(function(x) get_ncaa_park_factor(x,c(2016:2020))

pfs = bind_rows(pfs) %>% select(school,final_pf)

ACC = left_join(ACC,pfs,by="school")

####wRC plus formula####
ACC = ACC %>% mutate(wOBA = ((wBB*BB)+(wHBP*HBP)+(w1B*(H-`2B`-`3B`-HR))+(w2B*`2B`)+(w3B*`3B`)+(wHR*HR))/(PA-IBB),
                     wRAA = ((wOBA-lgwOBA)/wOBA_Scale)*PA,
                     wRC_plus = ((((wRAA/PA) + runsPA) + (runsPA-(final_pf*runsPA)))/(1662/9482))*100,
                     wOBA = round(wOBA,3),
                     wRAA = round(wRAA,3),
                     wRC_plus = round(wRC_plus)) %>% select(Player,school,PA,wOBA,wRAA,wRC_plus)
