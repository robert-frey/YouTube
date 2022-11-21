#RUN THE FIRST TWO LINES IF YOU DO NOT HAVE THESE PACKAGES INSTALLED
#install.packages('tidyverse')
#install.packages('baseballr')
library(tidyverse)
library(baseballr)

#acquire world series statcast data
world_series <- statcast_search(start_date = "2022-10-29", end_date = "2022-11-05")

#create a new column to get pitcher team
world_series <- world_series %>% mutate(pitch_team = ifelse(inning_topbot=="Top",home_team,away_team))

#get average velo by team and pitch type
world_series %>% group_by(pitch_team,pitch_name) %>% summarise(avg_velo = mean(release_speed,na.rm=T))

#create a function that returns a heatmap of a team and a pitch type
get_pitch_heatmap <- function(df,team,pitch_type) {
  
  if (pitch_type == "Breaking Ball") {
    pitch_df <- df %>% dplyr::filter(pitch_team == team & pitch_name %in% c("Slider","Curveball","Knuckle Curve"))
  } else if (pitch_type == "Fastball") {
    pitch_df <- df %>% dplyr::filter(pitch_team == team & pitch_name %in% c("4-Seam Fastball","Sinker","Cutter"))    
  } else if (pitch_type == "Offspeed") {
    pitch_df <- df %>% dplyr::filter(pitch_team == team & pitch_name %in% c("Changeup","Split-Finger"))
  }
  
  plot <- ggplot(pitch_df,aes(x=plate_x,y=plate_z)) +
    stat_density_2d(aes(fill = ..density..), geom="raster", contour = F) +
    scale_fill_distiller(type = "div") +
    xlim(c(-2,2)) +
    ylim(c(0,5)) +
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1) +
    theme_classic() +
    xlab('Horizontal Pitch Location') +
    ylab('Vertical Pitch Location') +
    ggtitle(paste(team,pitch_type,"Pitch Heatmap"),"Catcher's View")
  
  return(plot)
  
  
}

#Run the function
get_pitch_heatmap(world_series,"HOU","Fastball")
