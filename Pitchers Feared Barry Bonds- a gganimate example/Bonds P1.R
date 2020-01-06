#insert url
Bondsurl = "https://www.baseball-reference.com/players/gl.fcgi?id=bondsba01&t=b&year=2004"

#install.packages('plyr')
#install.packages('tidyverse')
library(plyr)
library(tidyverse)
urlid = list()

library(rvest)
urlid = Bondsurl %>%
  read_html() %>%
  html_nodes(xpath = "//td/a") %>%
  html_attr('href') 

#Convert to data frame and change column names
urlid = as.data.frame(urlid)
colnames(urlid)[1] = "Urls"

#Keep only boxscore urls and insert full url
urlid = urlid %>% filter(grepl("boxes", Urls))

urlid$BoxScores <- paste0("https://baseball-reference.com",urlid$Urls)

#get boxscores of every game Bonds appeared in 2004
bonds <- list()

for (i in 1:nrow(urlid)) {
  bonds[[i]] = urlid$BoxScores[i] %>% 
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node('#play_by_play') %>%
    html_table()
  bonds[[i]]$Date = urlid$BoxScores[i] %>% read_html() %>% html_nodes(".scorebox_meta div:nth-child(1)") %>% html_text
  
  Sys.sleep(0.5)
  
}

#Create Data Frame of Bonds' Boxscores
GameLogs = ldply(bonds, data.frame)

#Subset Data to only Barry Bonds's Plate Appearances
#install.packages('stringr')
library(stringr)
GameLogs$Batter = str_trim(GameLogs$Batter)
BondsPAs = GameLogs[grepl("Bonds",GameLogs$Batter),]


#Format the Date Column
BondsPAs$Date = format(as.Date(BondsPAs$Date, "%A, %B %d, %Y"), "%m/%d/%Y")
BondsPAs$Date = as.Date(BondsPAs$Date,format = "%m/%d/%Y")

#Get Play Results
BondsPAs$Result = word(BondsPAs$Play.Description, start = 1, end = 1)

#Remove Punctuation From Result column
?regex
BondsPAs$Result = gsub('[[:punct:] ]+',' ',BondsPAs$Result)

#Replace Various Strings
BondsPAs$Result = str_replace(BondsPAs$Result,"Home","Home Run")
BondsPAs$Result = str_replace(BondsPAs$Result,"Ground rule","Double")
BondsPAs$Result = str_replace(BondsPAs$Result,"Foul","Foul Out")
BondsPAs$Result = str_replace(BondsPAs$Result,"Intentional","Walk")
BondsPAs$Result = str_replace(BondsPAs$Result,"\\bGround\\b","Groundout")
BondsPAs$Result = str_replace(BondsPAs$Result,"Hit","HBP")

#check results
BondsPAs$Result = str_trim(BondsPAs$Result)
unique(BondsPAs$Result)

#Replace or remove strings
BondsPAs$Result = str_replace(BondsPAs$Result,"Reached","Reached on Error")
BondsPAs$Result = str_replace(BondsPAs$Result,"Fielder s","FC")
BondsPAs = BondsPAs[!grepl("Wild|Snow|Feliz|Durham|Grissom|Passed",BondsPAs$Result),]

#See count of each result type

BondsPAs %>% 
  group_by(Result) %>%
  plyr::summarize(result_type = plyr::count(Result))

#Convert Result to factor for plotting
BondsPAs$Result = as.factor(BondsPAs$Result)


#plotting vertical bar chart
#install.packages('gganimate')
library(gganimate)
#reorder from smallest to largest occurence
Bondsanim = ggplot(BondsPAs[grepl("-2-|-23|--3", BondsPAs$RoB),], aes(Result, fill = Result)) +     
  #..count.. is the count of each result
  geom_bar(aes(y=..count..)) +
  #display occurences of event
  
  #display occurences of event, expressed as percentage
  
  xlab("Result") +
  ggtitle(label="Barry Bonds PA Results with min. 1 Runner on Base and First Base Open",
          subtitle ="2004 Season") +
  #tranisition states that are ordered by smallest to largest occurences of Results
  #each 'state' is a static plot of each result
  #we reorder to again, order by smallest occurence of result to largest occurence of result
  
 
  
  #shadow mark allows the next plot in the stat to be 'layered' onto the previous plot
  shadow_mark() +
  theme(legend.position = "none")

#install.packages('gifski')
library(gifski)
#gif animation
animate(Bondsanim, 30, fps = 1,  width = 800, height = 800,
        renderer = gifski_renderer("Bonds.gif")) 

#plotting results by date
#cumulative sums of each result by each day
BondsPAs$value = 1

BondsCumulative = BondsPAs %>% 
  complete(Date, nesting(Result), fill=list(value=0)) %>% 
  arrange(Date) %>%  
  group_by(Result) %>%
  mutate(csum = cumsum(value))

#plotting
Bondsanim = 
  ggplot(BondsCumulative, 
         aes(x=Result, fill=Result)) +
  geom_col(aes(y=csum)) + 
  labs(x=NULL, y=NULL, fill=NULL, title="Barry Bonds PA Results by Date in 2004", subtitle = "{closest_state}") +
  #the "{closet_state}" indicates what state (Game Date) the results occured
              #flip coordinates for better aesthetics with animation
  
  #transitioning by each Game Date for the plot
   #visual aesthetics

#install.packages('av')
library(av)
#Video animation
animate(Bondsanim, 500, fps = 10,  width = 800, height = 800,
        renderer = av_renderer("Bonds.mp4")) 
