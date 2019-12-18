#install.packages('rvest')
#install.packages('plyr')
#install.packages('dplyr')
library(rvest)
library(plyr)
library(dplyr)

years = c(2013:2019)
urls = list()
for (i in 1:length(years)) {
  url = paste0('https://www.baseball-reference.com/register/league.cgi?group=NCAA&year=',years[i],'#all_league_batting')
  urls[[i]] = url
}

tbl = list()
years = 2013
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  tbl[[j]]$Year = years
  j = j+1
  years = years+1
}

NCAAbref = ldply(tbl, data.frame)
NCAAbref = NCAAbref[,c(-1,-3)]

Divisions = read.csv('Divisions.csv')

NCAAdata = join(NCAAbref, Divisions, by='Lg', type = 'left', match = 'first')
