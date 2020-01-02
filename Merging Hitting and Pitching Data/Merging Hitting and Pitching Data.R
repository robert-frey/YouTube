#install.packages('rvest')
#install.packages('plyr')
#install.packages('tidyverse')
#install.packages('plotly')
library(rvest)
library(plyr)
library(tidyverse)
library(plotly)

#scrape urls of each year
years = c(2013:2019)
urls = list()
for (i in 1:length(years)) {
  url = paste0("https://www.baseball-reference.com/register/league.cgi?group=NCAA&year=",years[i],"#all_league_batting")
  urls[[i]] = url
}

#scrape hyperlinks of each year's batting table
urlid = list()
years =2013
j = 1
for (j in seq_along(urls)) {
  urlid[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes(xpath = "//td/a") %>%
    html_attr('href') %>%
    as.data.frame()
  urlid[[j]]$Year = years
  j = j+1
  years = years +1
}

#convert list to dataframe and insert the correct url and conference
urlids = ldply(urlid, data.frame)
urlids$url = paste0('https://baseball-reference.com', urlids$.,'#all_league_batting') #correct url
for (i in 1:nrow(urlids)) {
  Conf = read_html(urlids$url[i]) %>% html_nodes("#meta span+ span") %>% html_text()
  urlids$Conference[i] = Conf #correct conference
}

#create another list to pull hitting data of each team within each conference
conftbl <- list()

for (i in 1:nrow(urlids)) {
  conftbl[[i]] = urlids$url[i] %>% 
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node('#league_batting') %>%
    html_table()
  conftbl[[i]]$Year = urlids$Year[i]
  conftbl[[i]]$Conference = urlids$Conference[i] 
  
  #lot of code to be ran, so please include the sys.sleep to ensure it is run to completion Â 
  Sys.sleep(0.5)
}

#convert to data frame and impute NAs in 'Tm' column with the team names in other columns
NCAAconfs = ldply(conftbl, data.frame)

NCAAconfs = NCAAconfs %>%
  mutate(Tm = coalesce(Tm, Coastal.Division, South.Division,Red.Rolfe.Division,
                       Western.Division, Southern.Division))

#check if there are any remaining NAs in the 'Tm' column
sum(is.na(NCAAconfs$Tm))

#remove unnecessary columns
NCAAconfs = NCAAconfs[,c(-2,-30:-34)]

#load in the correct divisions
Divisions = read.csv('Divisions.csv')
#change column name to Conference for the merge statement
colnames(Divisions)[1] = 'Conference'

#merge the two dataframe by Conference and Year and include all rows
NCAAConfData = merge(x=NCAAconfs,y=Divisions, by= c('Conference','Year'), all.x = TRUE)

#check if there are any NAs in Division
sum(is.na(NCAAConfData$Division))

#Remove rows that contain 'League Totals'
NCAAConfData = NCAAConfData[!grepl("League Totals",NCAAConfData$Tm),]
library(rvest)
library(plyr)
library(tidyverse)
library(plotly)

#scrape urls of each year
years = c(2013:2019)
urls = list()
for (i in 1:length(years)) {
  url = paste0("https://www.baseball-reference.com/register/league.cgi?group=NCAA&year=",years[i],"#all_league_pitching")
  urls[[i]] = url
}

#scrape hyperlinks of each year's batting table
urlid = list()
years =2013
j = 1
for (j in seq_along(urls)) {
  urlid[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes(xpath = "//td/a") %>%
    html_attr('href') %>%
    as.data.frame()
  urlid[[j]]$Year = years
  j = j+1
  years = years +1
}

#convert list to dataframe and insert the correct url and conference
urlids = ldply(urlid, data.frame)
urlids$url = paste0('https://baseball-reference.com', urlids$.,'#all_league_pitching') #correct url
for (i in 1:nrow(urlids)) {
  Conf = read_html(urlids$url[i]) %>% html_nodes("#meta span+ span") %>% html_text()
  urlids$Conference[i] = Conf #correct conference
}

#create another list to pull hitting data of each team within each conference
conftbl <- list()

for (i in 1:nrow(urlids)) {
  conftbl[[i]] = urlids$url[i] %>% 
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node('#league_pitching') %>%
    html_table()
  conftbl[[i]]$Year = urlids$Year[i]
  conftbl[[i]]$Conference = urlids$Conference[i] 
  
  #lot of code to be ran, so please include the sys.sleep to ensure it is run to completion Â 
  Sys.sleep(0.5)
}

#convert to data frame and impute NAs in 'Tm' column with the team names in other columns
NCAAPitchingconfs = ldply(conftbl, data.frame)

NCAAPitchingconfs = NCAAPitchingconfs %>%
  mutate(Tm = coalesce(Tm, Coastal.Division, South.Division,Red.Rolfe.Division,
                       Western.Division, Southern.Division))

#check if there are any remaining NAs in the 'Tm' column
sum(is.na(NCAAPitchingconfs$Tm))

#remove unnecessary columns
NCAAPitchingconfs = NCAAPitchingconfs[,c(-2,-36:-40)]

#load in the correct divisions
Divisions = read.csv('Divisions.csv')
#change column name to Conference for the merge statement
colnames(Divisions)[1] = 'Conference'

#merge the two dataframe by Conference and Year and include all rows
NCAAPitchConfData = merge(x=NCAAPitchingconfs,y=Divisions, by= c('Conference','Year'), all.x = TRUE)

#check if there are any NAs in Division
sum(is.na(NCAAPitchConfData$Division))

#Remove rows that contain 'League Totals'
NCAAPitchConfData = NCAAPitchConfData[!grepl("League Totals",NCAAPitchConfData$Tm),]

#Merge both data together
NCAAHitPitchData = merge(x=NCAAConfData,y=NCAAPitchConfData, by = c('Tm','Year'), all.x = TRUE)

#Remove all NA columns
NCAAHitPitchData = subset(NCAAHitPitchData, select = -c(IBB.x,IBB.y,GF,CG))

#Create D1 and D2 
D1Data = filter(NCAAHitPitchData, Division.x == 1)
D2Data = filter(NCAAHitPitchData, Division.x == 2)

#Check NAs
sum(is.na(D1Data))
sum(is.na(D2Data))

#remove NAs
D2Data = na.omit(D2Data)
