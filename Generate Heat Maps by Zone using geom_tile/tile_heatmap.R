library(tidyverse)

TM <- read_csv('https://raw.githubusercontent.com/robert-frey/YouTube/master/Generate%20Heat%20Maps%20by%20Zone%20using%20geom_tile/example_trackman.csv')

head(TM)


get_zone_data <- function(df, Player, type = "whiff") {
  
  df <- df %>% mutate(zone = case_when(px <= -0.28 & px >= -0.83 & pz >= 2.9 & pz <= 3.6 ~ 1,
                                       px > -0.28 & px < 0.28 & pz >= 2.9 & pz <= 3.6 ~ 2,
                                       px >= 0.28 & px <= 0.83 & pz >= 2.9 & pz <= 3.6 ~ 3,
                                       px <= -0.28 & px >= -0.83 & pz >= 2.2 & pz <= 2.9 ~ 4,
                                       px > -0.28 & px < 0.28 & pz >= 2.2 & pz <= 2.9 ~ 5,
                                       px >= 0.28 & px <= 0.83 & pz >= 2.2 & pz <= 2.9 ~ 6,
                                       px <= -0.28 & px >= -0.83 & pz >= 1.5 & pz <= 2.2 ~ 7,
                                       px > -0.28 & px < 0.28 & pz >= 1.5 & pz <= 2.2 ~ 8,
                                       px >= 0.28 & px <= 0.83 & pz >= 1.5 & pz <= 2.2 ~ 9,
                                       TRUE ~ NA_real_),
                      whiff = ifelse(pitchcall == "StrikeSwinging",1,0),
                      swing = ifelse(pitchcall %in% c("FoulBall","InPlay","StrikeSwinging"),1,0),
                      take = ifelse(pitchcall %in% c("BallCalled","StrikeCalled"),1,0)) %>%
    dplyr::filter(batter == Player) %>%
    dplyr::group_by(batter,zone) %>%
    dplyr::summarise(pitches = n(),
                     whiff = sum(whiff),
                     swing = sum(swing),
                     take = sum(take),
                     avg_ev = mean(ev,na.rm=T),
                     max_ev = max(ev,na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(whiff_pct = 100*round(whiff/swing,3),
                  take_pct = 100*round(take/swing,3),
                  swing_pct = 100*round(swing/pitches,3),
                  avg_ev = round(avg_ev,1),
                  max_ev = round(max_ev,1))
  
  if (type == "whiff") {
    
    z1 <- df %>% dplyr::filter(zone == 1) %>% dplyr::pull(whiff_pct)
    z2 <- df %>% dplyr::filter(zone == 2) %>% dplyr::pull(whiff_pct)
    z3 <- df %>% dplyr::filter(zone == 3) %>% dplyr::pull(whiff_pct)
    z4 <- df %>% dplyr::filter(zone == 4) %>% dplyr::pull(whiff_pct)
    z5 <- df %>% dplyr::filter(zone == 5) %>% dplyr::pull(whiff_pct)
    z6 <- df %>% dplyr::filter(zone == 6) %>% dplyr::pull(whiff_pct)
    z7 <- df %>% dplyr::filter(zone == 7) %>% dplyr::pull(whiff_pct)
    z8 <- df %>% dplyr::filter(zone == 8) %>% dplyr::pull(whiff_pct)
    z9 <- df %>% dplyr::filter(zone == 9) %>% dplyr::pull(whiff_pct)
    
    dat <- data.frame(x = c(-0.83,-0.83,-0.83,0,0,0,0.83,0.83,0.83),
                      y = c(2,3,4,2,3,4,2,3,4),
                      value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
    
    p <- ggplot(dat, aes(x = x, y = y, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = value), color = "white", size = 4) +
      scale_fill_gradient(low ="blue", high = "red") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key       = element_blank(),
            panel.background = element_rect(fill = "white"),
            legend.position = "None",
            complete = TRUE) +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      ggtitle(paste(Player,toupper(type),"Percentage"))
    
  } else if (type == "Average EV" | type == "Mean EV") {
    
    z1 <- df %>% dplyr::filter(zone == 1) %>% dplyr::pull(avg_ev)
    z2 <- df %>% dplyr::filter(zone == 2) %>% dplyr::pull(avg_ev)
    z3 <- df %>% dplyr::filter(zone == 3) %>% dplyr::pull(avg_ev)
    z4 <- df %>% dplyr::filter(zone == 4) %>% dplyr::pull(avg_ev)
    z5 <- df %>% dplyr::filter(zone == 5) %>% dplyr::pull(avg_ev)
    z6 <- df %>% dplyr::filter(zone == 6) %>% dplyr::pull(avg_ev)
    z7 <- df %>% dplyr::filter(zone == 7) %>% dplyr::pull(avg_ev)
    z8 <- df %>% dplyr::filter(zone == 8) %>% dplyr::pull(avg_ev)
    z9 <- df %>% dplyr::filter(zone == 9) %>% dplyr::pull(avg_ev)
    
    dat <- data.frame(x = c(-0.83,-0.83,-0.83,0,0,0,0.83,0.83,0.83),
                      y = c(2,3,4,2,3,4,2,3,4),
                      value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
    
    p <- ggplot(dat, aes(x = x, y = y, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = value), color = "white", size = 4) +
      scale_fill_gradient(low ="blue", high = "red") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key       = element_blank(),
            panel.background = element_rect(fill = "white"),
            legend.position = "None",
            complete = TRUE) +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      ggtitle(paste(Player,toupper(type),"Percentage"))
    
  }
  
  return(p)
  
}

get_zone_data(TM,"Example LHB", type = "whiff")
