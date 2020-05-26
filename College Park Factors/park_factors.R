devtools::install_github("BillPetti/baseballr")

library(baseballr)

school_id_lu("UConn")

get_ncaa_park_factor(164,c(2015:2019), type = "conference")
