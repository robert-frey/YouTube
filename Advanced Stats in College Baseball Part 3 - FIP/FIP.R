library(tidyverse)

#Load in CSV files
pitching = read.csv("sample_pitchers.csv", stringsAsFactors = F)

fip_guts = read.csv("fip_constants.csv", stringsAsFactors = F)

#use dplyr's left_join function to get the fip constants by year
pitching = left_join(pitching,fip_guts,by="year")

#adjust for outs made and divide by 3 of total innings pitched outs to get accurate FIP numbers
pitching = pitching %>% mutate(addl_outs = ###WATCH YOUTUBE VIDEO FOR CODE###,
                               ip_outs = ###WATCH YOUTUBE VIDEO FOR CODE###,
                               FIP = ((13*HR.A)+(3*(BB.A+HB))-(2*SO))/(ip_outs/3)+cFIP) %>% select(-IRS,-IR,-GP)
#rounding
pitching$FIP = round(pitching$FIP,2)

#sample query with a plot
pitching %>% filter(ip_outs >= 75,GS > 0) %>% select(year,Player,school,Yr,ERA,FIP) %>% 
  arrange(FIP) %>%
  mutate(Diff = ERA - FIP) %>% slice(1:10) %>%
  ggplot(aes(x=ERA,y=FIP,color=school)) +
  geom_point() +
  geom_text(aes(label=Player), vjust = -1)
