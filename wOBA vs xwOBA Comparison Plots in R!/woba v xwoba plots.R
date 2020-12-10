#devtools::install_github("BillPetti/baseballr)
library(baseballr)
#install.packages('tidyverse')
library(tidyverse)
#devtools::install_github("BayesBall/CalledStrike")
library(CalledStrike)
#install.packages('patchwork')
library(patchwork)

#sample dataset from the CalledStrike Packages
SC = sc_sample

#Some Plots
woba_plot(SC)

ewoba_plot(SC)

woba_contour(SC)

ewoba_contour(SC)

#Savant dataset from previous video, skip or use the sc_sample dataset if you don't have it
Savant = readr::read_csv("SavantHittingData19.csv")

#example of player name
woba_contour(Savant %>% filter(player_name == "Anthony Rendon"))

#create the function
comparison_plot <- function(player_name,seasons,type) {
  #### WATCH YOUTUBE VIDEO FOR CODE ####
  
  if (type == "plot" | type == "Plot") {
    xwOBA <- CalledStrike::ewoba_plot(player_data,title = paste(player_name,"xwOBA from",seasons[[1]],"-",max(seasons))) +
      theme_classic() +
      centertitle() +
      labs(fill = "xwOBA", caption = "data:baseballr,plot:CalledStrike")
    
    wOBA <- CalledStrike::woba_plot(player_data,title = paste(player_name,"wOBA from",seasons[[1]],"-",max(seasons))) +
      theme_classic() +
      centertitle() +
      labs(fill = "wOBA", caption = "data:baseballr,plot:CalledStrike")
  } else if (type == "Contour" | type == "contour") {
    xwOBA <- CalledStrike::ewoba_contour(player_data,title = paste(player_name,"xwOBA from",seasons[[1]],"-",max(seasons))) +
      theme_classic() +
      centertitle() +
      labs(fill = "xwOBA", caption = "data:baseballr,plot:CalledStrike")
    
    wOBA <- CalledStrike::woba_contour(player_data,title = paste(player_name,"wOBA from",seasons[[1]],"-",max(seasons))) +
      theme_classic() +
      centertitle() +
      labs(fill = "wOBA", caption = "data:baseballr,plot:CalledStrike")
  }
  
  return(wOBA + xwOBA)
}

comparison_plot("Marcell Ozuna",seasons = c(2017:2020), type = "contour")
