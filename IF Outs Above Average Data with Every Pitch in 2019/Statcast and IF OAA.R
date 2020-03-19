#load in CSV files
Savant = read.csv("2019SavantPitchingData.csv", stringsAsFactors = F)
IF = read.csv("infield-oaa.csv", stringsAsFactors = F)

#load in library
library(tidyverse)

#select and pull if outs above average by each position
FB = IF %>% filter(primary_pos_formatted == "1B") %>% mutate(fielder_3 = player_id) %>% select(fielder_3, outs_above_average)
SB = IF %>% filter(primary_pos_formatted == "2B") %>% mutate(fielder_4 = player_id) %>% select(fielder_4, outs_above_average)
TB = IF %>% filter(primary_pos_formatted == "3B") %>% mutate(fielder_5 = player_id) %>% select(fielder_5, outs_above_average)
SS = IF %>% filter(primary_pos_formatted == "SS") %>% mutate(fielder_6 = player_id) %>% select(fielder_6, outs_above_average)

#pull the column of numbers that match the position
FBOAA = merge(x=Savant %>% select(fielder_3),y=FB, by = "fielder_3", all.x = TRUE) %>% mutate(fielder_3_OAA = outs_above_average) %>% select(fielder_3_OAA) %>% pull(var = fielder_3_OAA)
SBOAA = merge(x=Savant %>% select(fielder_4),y=SB, by = "fielder_4", all.x = TRUE) %>% mutate(fielder_4_OAA = outs_above_average) %>% select(fielder_4_OAA) %>% pull(var = fielder_4_OAA)
TBOAA = merge(x=Savant %>% select(fielder_5),y=TB, by = "fielder_5", all.x = TRUE) %>% mutate(fielder_5_OAA = outs_above_average) %>% select(fielder_5_OAA) %>% pull(var = fielder_5_OAA)
SSOAA = merge(x=Savant %>% select(fielder_6),y=SS, by = "fielder_6", all.x = TRUE) %>% mutate(fielder_6_OAA = outs_above_average) %>% select(fielder_6_OAA) %>% pull(var = fielder_6_OAA)

#Add data to Savant
Savant = Savant %>% add_column(fielder_3_oaa = FBOAA, fielder_4_oaa = SBOAA,
                               fielder_5_oaa = TBOAA, fielder_6_oaa = SSOAA)

#Random 10 Observations
Sample = Savant %>% sample_n(10)

#view column classes
sapply(Savant, class)

#convert the oaa columns to numeric
Savant[,91:94] = lapply(Savant[,91:94], as.numeric)
###BE SURE TO RUN LINE 29 AFTER RUNNING THIS CODE TO ENSURE COLUMNS WERE CONVERTED###

#Replace NAs and sum the total infield outs above average for each pitch
Savant = Savant %>% mutate(if_oaa = rowSums(.[,91:94], na.rm = T))


#top if_oaa for each pitch
Sample = Savant %>% arrange(desc(if_oaa))

#View first 10 observations
Sample = head(Savant,10)

#Max and minimum for a given pitch
max(Savant$if_oaa)

min(Savant$if_oaa)


