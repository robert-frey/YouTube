#read csv from last video
D1IDs = read.csv("IDs2020D1.csv", stringsAsFactors = F)

library(baseballr)
library(tidyverse)
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
#Use Totals Row for Team Data
D1TBat = D1Bat %>% filter(PA > 0, grepl("Totals|Opponent Totals", Player))
D1TPitch = D1Pitch %>% filter(BF > 0, grepl("Totals|Opponent Totals", Player))

D1TBat = D1TBat %>% filter(PA > 0, !grepl("\\bOpponent Totals\\b", Player))
D1TPitch = D1TPitch %>% filter(BF > 0, !grepl("\\bOpponent Totals\\b", Player))

#Create new Data Frame Columns
D1TBat$`R/PA` <- round(D1TBat$R/D1TBat$PA,3)
D1TBat$`K%` <- round(D1TBat$K/D1TBat$PA,3) 
D1TPitch$`R/BF` <- round(D1TPitch$R/D1TPitch$BF,3)
D1TPitch$`SO%` <- round(D1TPitch$SO/D1TPitch$BF,3)

#Merge Hitting and Pitching Data
D1Data = merge(x=D1TBat,y=D1TPitch %>% select(school,`R/BF`,`SO%`),by="school",all.x=T)

#Two sources to pull ncaa logo data
#https://github.com/lbenz730/ncaahoopR
#devtools::install_github("lbenz730/ncaahoopR")

#https://github.com/snestler/wncaahoopR
#devtools::install_github("snestler/wncaahoopR")

#Get logos from wncaahoopR packages
library(wncaahoopR)
colors = wncaahoopR::ncaa_colors %>% select(ncaa_name,logo_url,primary_color) %>% rename(school = ncaa_name)

D1logos = merge(x=D1IDs,y=colors,by="school",all.x =T) %>% select(school,logo_url,primary_color)

#check for NAs
sum(is.na(D1logos$logo_url))

#Merge Data with logos
D1Data = merge(x=D1Data,y=D1logos,by="school",all.x = T)

#Fill Dallas Baptist's missing logo with logo
D1Data$logo_url[55] <- "https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Dallas_Baptist_Patriots_wordmark.svg/320px-Dallas_Baptist_Patriots_wordmark.svg.png"

#Run Differential per PA
D1Data$RD <- D1Data$`R/PA` - D1Data$`R/BF`

#Install ggimage, lot of below code came from @statsowar's CFB logo example
#install.packages('ggimage')
library(ggimage)
#https://gist.github.com/spfleming/d4589e0a25893dbb5461550ea9a25d54
#Venmo eithe of us!
#Parker's venmo: @Parker-Fleming-2
#My venmo: @Robert-Frey-7

#Create a function to generate logo plots
per_pa_plot <- function(conference = "all"){
  #######WATCH MY YOUTUBE VIDEO FOR CODE#######
  #######WATCH MY YOUTUBE VIDEO FOR CODE#######
  ggplot(Filt,aes(x=`R/BF`,y=`R/PA`)) +
    geom_image(image = Filt$logo_url, asp = 16/9) +
    labs(x = "Runs Allowed/PA",
         y = "Runs Scored/PA",
         caption = "Figure @RobertFrey40 | Data: baseballr",
         title = paste0("Team Efficiency in ", Title),
         subtitle = "2020 season through 03/09/2020") +
    theme_bw() +
    theme(axis.text = element_text(size = 8),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(size = 10))
}

#run custom function of code, so all conferences or something like the SEC, Big Ten, ACC, etc.
per_pa_plot(conference = "all")
