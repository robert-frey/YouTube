#Read in csv
BondsPAs <- read.csv("BondsPAs.csv", stringsAsFactors = F)

#Keep only pitches as a tibble
library(tidyverse)
PitchType = as.tibble(BondsPAs$Pit.cnt.)

#Remove parentheses,punctuation, and digits
#?regex

PitchType$Code = gsub(".*)","",PitchType$value)
PitchType$Code = gsub('[[:punct:] ]+','',PitchType$Code)
PitchType$Code = gsub('[[:digit:]]','', PitchType$Code)

#Check for strings
library(stringr)
sum(str_count(PitchType$Code,"1|2|3"))
sum(str_count(PitchType$Code,"L"))
sum(str_count(PitchType$Code,"M|N|O|Q|R|U|Y"))

#Replace L string with C
PitchType$Code = str_replace(PitchType$Code,"L","C")

#Count Balls and Strikes
PitchType$Balls = str_count(PitchType$Code,"B|I|H|P|V")
PitchType$Strikes = str_count(PitchType$Code,"C|S|F|X|T|K")

#Create Data Frame that displays balls and strikes
BondsBvS = data.frame(
  Player = "Barry Bonds",
  Balls = as.numeric(sum(PitchType$Balls)),
  Strikes = as.numeric(sum(PitchType$Strikes)),
  PAs = 617,
  OBP = .609,
  Year = 2004
)

BondsBvS$Bprob = (1-BondsBvS$Sprob)

#Add Strike and Ball Probabilities
BondsBvS$Sprob = round(BondsBvS$Strikes/(BondsBvS$Balls+BondsBvS$Strikes),3)
BondsBvS$Bprob = round(BondsBvS$Balls/(BondsBvS$Balls+BondsBvS$Strikes),3)

#Measure Bonds's OBP by randomly selecting Balls and Strikes
Balls = 0
Strikes = 0
j = 1

Outcomes = data.frame(
  Outcome = character(),
  stringsAsFactors = FALSE
)
#random.sample <- function(x) {
repeat {
  # do something
  result = sample(c("Ball","Strike"),size=1,
                  replace = TRUE, prob = c(BondsBvS$Bprob,BondsBvS$Sprob))
  for (i in 1:length(BondsBvS$PAs)) {
    if (j == BondsBvS$PAs+1){
      return(print(paste0(BondsBvS$Player," would have an OBP of ",
                          round(sum(str_count(Outcomes$Outcome,"Walk"))/(BondsBvS$PAs),3),
                          " without a baseball bat in ", BondsBvS$Year,".",
                          " His actual OBP in ", BondsBvS$Year, " was ",
                          BondsBvS$OBP)))
    }
    else if (result == "Ball") {
      
    }
    else if (result == "Strike") {
      
    }
    
    
    #Walk if Balls = 4 and Resets count
    if (Balls == 4) {
      
      
      
      
    }
    #Strikeout if Strikes = 3 and Resets count
    else if (Strikes == 3) {
      
      
      
      
    }
    
  }
}
