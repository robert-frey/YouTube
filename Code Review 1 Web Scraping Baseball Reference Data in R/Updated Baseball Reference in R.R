#install.packages('rvest')
#install.packages('tidyverse')
library(rvest)
library(tidyverse)

years <- c(2013:2021)
urls <-  list()
for (i in 1:length(years)) {
  url = paste0('https://www.baseball-reference.com/register/league.cgi?group=College&year=',years[i])
  urls[[i]] = url
}

urls <- urls %>% unlist() %>% as.data.frame() %>%
  dplyr::mutate(year = years) %>% dplyr::rename(url = ".")

scrape_bref_college_data <- function(url, year, table_num) {
  
  if( table_num == 1) {
  df <- url %>% xml2::read_html() %>% rvest::html_nodes("table") %>%
    rvest::html_table(trim=T) %>% .[[1]] %>%
    dplyr::mutate(Year = year)
  } else {
    df <- url %>% xml2::read_html() %>%
      rvest::html_nodes(xpath = '//comment()') %>%
      rvest::html_text() %>%
      paste(collapse='') %>%
      xml2::read_html() %>%
      rvest::html_node('#league_pitching') %>%
      rvest::html_table(trim=T) %>%
      dplyr::mutate(Year = year)
  }
  
  df
}

scrape_bref_college_data(urls$url[9],2021,2)

teams <- 1:nrow(urls) %>% purrr::map_df(function(x) scrape_bref_college_data(urls$url[x],urls$year[x],1))

conference_pitching <- 1:nrow(urls) %>% purrr::map_df(function(x) scrape_bref_college_data(urls$url[x],urls$year[x],2))

conference_pitching <- conference_pitching %>% dplyr::select(-GF,-CG,-`SO/W`)

Divisions <- readr::read_csv("https://raw.githubusercontent.com/robert-frey/YouTube/master/Scraping%20Baseball%20Reference%20Data%20with%20R/Divisions.csv")

conference_pitching <- dplyr::left_join(conference_pitching, Divisions,by=c("Lg","Year"))

readr::write_csv(teams,"bref_conference_hitting.csv")
readr::write_csv(conference_pitching,"bref_conference_pitching.csv")

test <- readr::read_csv("bref_conference_hitting.csv")
