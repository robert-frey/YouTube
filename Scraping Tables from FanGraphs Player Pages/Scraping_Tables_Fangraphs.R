#install.packages('tidyverse')
library(tidyverse)
#install.packages('rvest')
library(rvest)
#install.packages('xml2')
library(xml2)
#install.packages('readr')
library(readr)

#use legacy url, Mike Trout is the example
url = "https://www.fangraphs.com/statss-legacy.aspx?playerid=10155&position=PB"

#List of Tables that appear on Mike Trout's player page
p = url %>% read_html() %>% html_nodes("table")

#Select the first table of Mike Trout's stats
url %>% read_html() %>% html_nodes("table") %>% .[13] %>% html_table(trim = T)

#create a vector of projections
projs = c("ATC","THE BAT","Depth Charts","Steamer","ZiPS","Average","THE BAT X")

#Select the first table of Mike Trout's stats and remove Postseason Data, Minor League Data, and Projections
Trout = url %>% read_html() %>% html_nodes("table") %>% .[13] %>% html_table(trim = T) %>%
  data.frame(stringsAsFactors = F) %>% dplyr::filter(!Team %in% projs, str_detect(Team,####WATCH YOUTUBE VIDEO FOR CODE###)==FALSE,
                                                     Season != "Postseason")

#Select the 'Advanced' Table 
TroutAdv = url %>% read_html() %>% html_nodes("table") %>% .[15] %>% html_table(trim = T) %>%
  data.frame(stringsAsFactors = F) %>% dplyr::filter(!Team %in% projs, str_detect(Team,####WATCH YOUTUBE VIDEO FOR CODE###)==FALSE,
                                                     Season != "Postseason")

#Combine the two tables
Trout_Adv = left_join(#WATCH YOUTUBE VIDEO FOR CODE#)

#Jacob DeGrom url
pitch_url = "https://www.fangraphs.com/statss-legacy.aspx?playerid=10954&position=P"

#pull both the first table and the advanced table
pitch_url %>% read_html() %>% html_nodes("table") %>% .[13] %>% html_table(trim = T) %>%
  data.frame(stringsAsFactors = F) %>% dplyr::filter(!Team %in% projs, str_detect(Team,####WATCH YOUTUBE VIDEO FOR CODE###)==FALSE,
                                                     Season != "Postseason")


