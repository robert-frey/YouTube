library(baseballr)
library(dplyr)
library(RSQLite)
library(DBI)

df <- statcast_search(start_date = Sys.Date()-1, end_date =Sys.Date(), player_type = "pitcher")

chadwick <- chadwick_player_lu()

chadwick <- chadwick |> dplyr::select(key_mlbam,name_first,name_last) |>
  dplyr::mutate(batter_name = paste0(name_last,", ",name_first)) |>
  dplyr::select(batter = key_mlbam,batter_name)

df <- left_join(df,chadwick,by="batter") |>
  dplyr::select(pitch_type:release_pos_z,batter_name,pitcher_name=player_name,everything())

df <- df |> tibble()

sdb <- dbConnect(drv = SQLite(),"statcast_db.sqlite")

dbWriteTable(conn = sdb, name = "statcast_data", value = df, overwrite = F, append = T)

dbDisconnect(sdb)
