#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages('tidyverse')
library(tidyverse)

#Filter to just d1 schools
d1_schools <- master_ncaa_team_lu %>% dplyr::filter(division == 1, year == 2021)

#error handling
safe_ncaa_scrape <- purrr::safely(ncaa_scrape)

#custom function to acquire baseball stats
ncaa_scraper <- function(schoolid,school,type) {
  if (type == "batting") {
    message(paste("Getting Batting Stats for", school))
    
    stats <- safe_ncaa_scrape(teamid = schoolid, year = 2021, type = "batting")
  } else {
    message(paste("Getting Pitching Stats for", school))
    
    stats <- safe_ncaa_scrape(teamid = schoolid, year = 2021, type = "pitching")
  }
  
  Sys.sleep(sample(seq(.005,.02,.001),1))
  
  return(stats)
}

#Map through hitting and pitching stats
batting_stats <- 1:nrow(d1_schools) %>% purrr::map(function(x) ncaa_scraper(d1_schools$school_id[x],
                                                                            d1_schools$school[x],
                                                                            type = "batting"))

pitching_stats <- 1:nrow(d1_schools) %>% purrr::map(function(x) ncaa_scraper(d1_schools$school_id[x],
                                                                            d1_schools$school[x],
                                                                            type = "pitching"))

#convert list to dataset
d1_batting_stats <- batting_stats %>% ####WATCH YOUTUBE VIDEO FOR CODE#####
  bind_rows()

d1_pitching_stats <- pitching_stats %>% ####WATCH YOUTUBE VIDEO FOR CODE####
  bind_rows()

#Remove Totals and Opponent Totals
d1_batting_stats <- d1_batting_stats %>% dplyr::filter(str_detect(Player,"(Totals)")==FALSE,
                                                       str_detect(Player,"(Opponent Totals)")==FALSE)

d1_pitching_stats <- d1_pitching_stats %>% dplyr::filter(str_detect(Player,"(Totals)")==FALSE,
                                                       str_detect(Player,"(Opponent Totals)")==FALSE)

#write file to CSV
readr::write_csv(d1_batting_stats,"d1_batting_0308.csv")

readr::write_csv(d1_pitching_stats,"d1_pitching_0308.csv")