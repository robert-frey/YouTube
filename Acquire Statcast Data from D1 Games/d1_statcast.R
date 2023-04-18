library(baseballr)
library(tidyverse)

# Acquiring the list of dates during the D1 college baseball
dates <- seq.Date(as.Date("2023-02-17"),as.Date("2023-04-17"),by="day")

#Acquire the game pks to scrape the statcast data
game_pks <- 1:length(dates) %>% purrr::map(function(x) mlb_game_pks(dates[x],22))

game_pks <- game_pks %>% bind_rows() %>% pull(game_pk)

#Create an error handler in case a game play-by-play data
safe_pbp <- purrr::safely(mlb_pbp)

#Acquire the statcast data of D1 games in a LIST
dat <- 1:length(game_pks) %>% purrr::map(function(x) safe_pbp(game_pks[x]), .progress = T)

#Map the data to a table
d1_statcast <- dat %>% map('result') %>% bind_rows()

#Adjust column names and change column names for better understanding
d1_statcast <- d1_statcast %>%
  select(game_pk,game_date,
         atBatIndex,
         pitchNumber,
         pitch_type = details.type.code,
         pitch_name = details.type.description,
         relspeed = pitchData.startSpeed #pitch velo,
         relspin = pitchData.breaks.spinRate #pitch spin,
         extension = pitchData.extension,
         spin_dir = pitchData.breaks.spinDirection,
         plate_time = pitchData.plateTime,
         zone = pitchData.zone,
         balls = count.balls.start,
         strikes = count.strikes.start,
         outs = count.outs.start,
         inning = about.inning,
         inning_topbot = about.isTopInning,
         description = details.description,
         batter = matchup.batter.fullName,
         pitcher = matchup.pitcher.fullName,
         batter_side = matchup.batSide.code,
         pitcher_hand = matchup.pitchHand.code,
         event = details.event,
         type = details.code,
         exit_speed = hitData.launchSpeed,
         launch_angle = hitData.launchAngle,
         distance = hitData.totalDistance,
         trajectory = hitData.trajectory,
         bb_hardness = hitData.hardness,
         bb_location = hitData.location,
         hit_coord_x = hitData.coordinates.coordX,
         hit_coord_y = hitData.coordinates.coordY,
         result_event = result.event,
         result_event_type = result.eventType,
         rbi = result.rbi,
         away_score = result.awayScore,
         home_score = result.homeScore,
         away_team, home_team,
         platelocside = pitchData.coordinates.pX,
         platelocheight = pitchData.coordinates.pZ,
         vx0 = pitchData.coordinates.vX0,
         vy0 = pitchData.coordinates.vY0,
         vz0 = pitchData.coordinates.vZ0,
         ax = pitchData.coordinates.aX,
         ay = pitchData.coordinates.aY,
         az = pitchData.coordinates.aZ,
         x0 = pitchData.coordinates.x0,
         y0 = pitchData.coordinates.y0,
         z0 = pitchData.coordinates.z0
  ) %>%
  mutate(atBatIndex = as.numeric(atBatIndex),
         atBatIndex = atBatIndex + 1,
         inning_topbot = ifelse(inning_topbot == T, "Top","Bot"),
         pitch_team = ifelse(inning_topbot == "Top",home_team,away_team),
         bat_team = ifelse(inning_topbot == "Bot",home_team,away_team)) %>%
  dplyr::filter(!is.na(pitchNumber)) %>%
  dplyr::arrange(game_pk,atBatIndex,pitchNumber)

#see which teams have data
unique(d1_statcast$pitch_team)

# use the TCU Horned frogs as an example
tcu <- d1_statcast %>% dplyr::filter(pitch_team == "TCU Horned Frogs")

#Create a strike zone
topKzone = 3.5
botKzone = 1.6
inKzone = -.95
outKzone = 0.95
kZone = data.frame(x = c(inKzone,inKzone,outKzone,outKzone,inKzone),
                   y = c(botKzone,topKzone,topKzone,botKzone,botKzone))

# Plot a basic heatmap
ggplot(tcu,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
  scale_fill_gradientn(colours = c("blue","red")) +
  lims(x = c(-2.5,2.5), y=c(0,6)) +
  geom_path(aes(x,y), data = kZone) +
  theme_classic() +
  xlab("Horizontal Pitch Location") +
  ylab("Vertical Pitch Location") +
  ggtitle("TCU Pitch Heatmap", subtitle = "Catcher's Perspective")

# See what pitch types are available
unique(tcu$pitch_name)

#Create a heatmap function that displays a heatmap of a team's pitches in counnts
pitch_heatmap_function <- function(tm, pitch, cnt) {
  
  df <- d1_statcast %>% dplyr::mutate(count = paste0(balls,"-",strikes)) %>%
    dplyr::filter(pitch_team == tm,
                                      pitch_name %in% pitch,
                                      count %in% cnt)
  
  ggplot(df,aes(x = platelocside, y = platelocheight)) +
    stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
    scale_fill_gradientn(colours = c("blue","red")) +
    lims(x = c(-2.5,2.5), y=c(0,6)) +
    geom_path(aes(x,y), data = kZone) +
    theme_classic() +
    xlab("Horizontal Pitch Location") +
    ylab("Vertical Pitch Location") +
    ggtitle(paste0(tm,"Pitch Heatmap of ",pitch," in ",cnt," Counts"), subtitle = "Catcher's Perspective")
  
}

pitch_heatmap_function("TCU Horned Frogs",c("Sinker"),"0-0")
