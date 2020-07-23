#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages("DBI")
library(DBI)
#install.packages("RSQLite")
library(RSQLite)
#install.packages("lubridate")
library(lubridate)

db = dbConnect(SQLite(),"statcast_db.sqlite")

statcast_write <- function() {
  day = lubridate::today()-1
  dat = baseballr::scrape_statcast_savant(start_date = day, end_date = day)
  dbWriteTable(db,"statcast_hitting",dat,overwrite = F, row.names = F, append = T)
  dbDisconnect(db)
}

statcast_write()
