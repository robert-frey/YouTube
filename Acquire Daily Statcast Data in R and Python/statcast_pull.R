#install.packages('baseballr')
library(baseballr)
#install.packages('DBI')
library(DBI)
#install.packages('RSQLite')
library(RSQLite)

#Acquire the previous days' data
sc <- baseballr::statcast_search()

# convert data to dataframe
sc <- data.frame(sc, stringsAsFactors = F)

#Connect to DB and add data
conn <- dbConnect(SQLite(), "statcast2023.sqlite")

dbWriteTable(conn = conn, "statcast_data_2023", sc, overwrite = F, append = T, rownames = F)

dbDisconnect(conn = conn)

# Test data to see if it's there
#conn <- dbConnect(SQLite(), "statcast2023.sqlite")

#dbGetQuery(conn = conn, "SELECT * FROM statcast_data_2023 LIMIT 10")

#dbDisconnect(conn = conn)
