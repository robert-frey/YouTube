#devtools::install_github('saberpowers/sabRmetrics')
library(sabRmetrics)
#install.packages('parallelly')
library(parallelly)
library(dplyr)
library(httr)
library(tictoc)

availableWorkers()

cluster <- parallelly::makeClusterPSOCK(12)
tic()

savant_data <- sabRmetrics::download_baseballsavant(
  start_date = "2024-03-28",
  end_date = "2024-09-30",
  cl = cluster
)

pbp <- sabRmetrics::download_statsapi("2024-03-28","2024-09-30", level = "mlb", game_type = "R", cl = cluster)

toc()

mlb_pbp <- pbp[['pitch']] %>%
  dplyr::select(game_id,at_bat_number = event_index, pitch_number, play_id) %>%
  dplyr::mutate(at_bat_number = at_bat_number + 1)

savant_data <- left_join(savant_data,mlb_pbp,by=c("game_id","at_bat_number","pitch_number"))

s500 <- savant_data %>% head(500)

httr::BROWSE(sabRmetrics::get_video_url(mlb_pbp$play_id[1]))
