#devtools::install_github("BillPetti/baseballr)
library(baseballr)
library(tidyverse)

#Get all D1 teams from D1Baseball.com
D1 = read_html("https://d1baseball.com/teams") %>% html_nodes(".team") %>% html_text %>%
  data.frame(stringsAsFactors = F)
colnames(D1)[1] <- "school"
D1$school = str_squish(D1$school)


#replace strings to correctly pull teamIDs
D1$school = str_replace(D1$school,"UT Rio Grande Valley","UTRGV")
D1$school = str_replace(D1$school,"State","St.")
D1$school = str_replace(D1$school,"NC St.","NC State")
D1$school = str_replace(D1$school,"Northern Colorado","Northern Colo.")
D1$school = str_replace(D1$school,"UL Monroe","La.-Monroe")
D1$school = str_replace(D1$school,"Georgia Southern","Ga. Southern")
D1$school = str_replace(D1$school,"Western Illinois","Western Ill.")
D1$school = str_replace(D1$school,"Mississippi Valley St.","Mississippi Val.")
D1$school = str_replace(D1$school,"Charleston Southern","Charleston So.")
D1$school = str_replace(D1$school,"Arkansas-Pine Bluff","Ark.-Pine Bluff")
D1$school = str_replace(D1$school,"Alcorn St.","Alcorn")
D1$school = str_replace(D1$school,"South Florida","South Fla.")
D1$school = str_replace(D1$school,"Alcorn St.","Alcorn")
D1$school = str_replace(D1$school,"Florida Gulf Coast","FGCU")
D1$school = str_replace(D1$school,"New Jersey Tech","NJIT")
D1$school = str_replace(D1$school,"Alcorn St.","Alcorn")
D1$school = str_replace(D1$school,"North Alabama","North Ala.")
D1$school = str_replace(D1$school,"College of Charleston","Col. of Charleston")
D1$school = str_replace(D1$school,"Cal St. Northridge","CSUN")
D1$school = str_replace(D1$school,"UNC Wilmington","UNCW")
D1$school = str_replace(D1$school,"Florida Atlantic","Fla. Atlantic")
D1$school = str_replace(D1$school,"Middle Tennessee","Middle Tenn.")
D1$school = str_replace(D1$school,"Florida International","FIU")
D1$school = str_replace(D1$school,"Western Kentucky","Western Ky.")
D1$school = str_replace(D1$school,"Illinois-Chicago","UIC")
D1$school = str_replace(D1$school,"Northern Kentucky","Northern Ky.")
D1$school = str_replace(D1$school,"Pennsylvania","Penn")
D1$school = str_replace(D1$school,"Central Michigan","Central Mich.")
D1$school = str_replace(D1$school,"Eastern Michigan","Eastern Mich.")
D1$school = str_replace(D1$school,"Northern Illinois","Northern Ill.")
D1$school = str_replace(D1$school,"Western Michigan","Western Mich.")
D1$school = str_replace(D1$school,"North Carolina A&T","N.C. A&T")
D1$school = str_replace(D1$school,"North Carolina Central","N.C. Central")
D1$school = str_replace(D1$school,"Dallas Baptist","DBU")
D1$school = str_replace(D1$school,"Southern Illinois","Southern Ill.")
D1$school = str_replace(D1$school,"Long Island","LIU")
D1$school = str_replace(D1$school,"Eastern Illinois","Eastern Ill.")
D1$school = str_replace(D1$school,"Eastern Kentucky","Eastern Ky.")
D1$school = str_replace(D1$school,"SIU Edwardsville","SIUE")
D1$school = str_replace(D1$school,"Southeast Missouri St.","Southeast Mo. St.")
D1$school = str_replace(D1$school,"Tennessee-Martin","UT Martin")
D1$school = str_replace(D1$school,"East Tennessee St.","ETSU")
D1$school = str_replace(D1$school,"Western Carolina","Western Caro.")
D1$school = str_replace(D1$school,"Central Arkansas","Central Ark.")
D1$school = str_replace(D1$school,"Incarnate Word","UIW")
D1$school = str_replace(D1$school,"Southeastern Louisiana","Southeastern La.")
D1$school = str_replace(D1$school,"Stephen F. Austin","SFA")
D1$school = str_replace(D1$school,"Texas A&M-Corpus Christi","A&M-Corpus Christi")
D1$school = str_replace(D1$school,"Loyola Marymount","LMU")
D1$school[9] = "UConn"
D1$school[183] = "Central Conn. St."
D1$school[262] = "Southern U."


#Get List of D1 Schools
D1list <- list()
for (i in 1:nrow(D1)) {
  D1list[[i]] = baseballr::school_id_lu(D1$school[i]) %>% filter(year==2020) %>% slice(1)
}

#Specific pull for certain areas
D1list[[114]] = school_id_lu("Marshall") %>% filter(year==2020) %>% slice(2)
D1list[[153]] = school_id_lu("Miami") %>% filter(year==2020) %>% slice(2)
D1list[[285]] = school_id_lu("Pacific") %>% filter(year==2020) %>% slice(4)
D1list[[287]] = school_id_lu("Portland") %>% filter(year==2020) %>% slice(2)

#place all ids into one data.frame
D1IDs = plyr::ldply(D1list,data.frame)

#write csv file of SchoolIDs
write.csv(D1IDs,"IDs2020D1.csv", row.names = F)

#get list of hitting and pitching data by team
Batting <- list()
Pitching <- list()
for (i in 1:nrow(D1IDs)) {
  #tryCatch function overwrites the error of no data availabe (if any) to go to the next school IDs
  tryCatch({
  Batting[[i]] <- ncaa_scrape(D1IDs$school_id[i], year = D1IDs$year[i], type = "batting")
  Pitching[[i]] <- ncaa_scrape(D1IDs$school_id[i], year = D1IDs$year[i], type = "pitching")
  },error=function(e){})
}

#call Batting into a Data Frame
D1Bat = plyr::ldply(Batting, data.frame)

#call Pitching into a Data Frame
D1Pitch = plyr::ldply(Pitching, data.frame)

#Replace NA with 0
D1Bat[is.na(D1Bat)] <- 0
D1Pitch[is.na(D1Pitch)] <- 0

#Create PA and OPS column
D1Bat$PA = D1Bat$AB+D1Bat$BB+D1Bat$HBP+D1Bat$SF+D1Bat$SH
D1Bat$OPS = D1Bat$OBPct+D1Bat$SlgPct

#Remove Totals and Opponent Totals rows and filter by PA or BF
D1Bat = D1Bat %>% filter(PA > 0, !grepl("Totals|Opponent Totals", Player))
D1Pitch = D1Pitch %>% filter(BF > 0, !grepl("Totals|Opponent Totals", Player))

library(RSQLite)
library(DBI)

#connecting a database
db = dbConnect(SQLite(), dbname ="D1Data.sqlite")

#writing the D1 Hitting and Pitching table we created to the database
dbWriteTable(conn = db, name = "D1_Hitting", D1Bat, overwrite=T, row.names = F)
dbWriteTable(conn = db, name = "D1_Pitching", D1Pitch, overwrite=T, row.names = F)