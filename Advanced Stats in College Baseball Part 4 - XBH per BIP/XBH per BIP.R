### XBH/BIP
### Extra Base Hits / Balls in Play
### XBH/BIP attempts to measure how frequent a pitcher gives up hard hits,
### since we do not have Exit Velocity data at the college level (at 
### least publicity), we have to do
### our best to 'measure' how often a pitcher gets hit hard. Generally,
### extra base hits are considered hard hit balls, so we will treat them as such.

#Load in Libraries
library(baseballr)
library(dplyr)

#Lookup Schools for teamid
school_id_lu("Cal Poly")
school_id_lu("Colorado Mesa")
school_id_lu("Coe")

#D1 - Cal Poly, ncaa_scrape function allows us to get pitching data
D1 = ncaa_scrape(90,2019,type="pitching")
#At least 1 apperance on the season
D1 = D1 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D1 = D1 %>% tidyr::replace_na(list(`2B-A`=0,`3B-A`=0,`HR-A`=0,`P-OAB`=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D1 = D1 %>% mutate(`P-OAB` = as.numeric(`P-OAB`))
#Calculate XBH per BIP
D1 = D1 %>% mutate(`XBH_BIP` = ###WATCH YOUTUBE VIDEO FOR CODE###,
                   `XBH_BIP` =  round(`XBH_BIP`,3))

#D2 - Colorado Mesa, ncaa_scrape function allows us to get pitching data
D2 = ncaa_scrape(11416,2019,type="pitching")
#At least 1 apperance on the season
D2 = D2 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D2 = D2 %>% tidyr::replace_na(list(`2B-A`=0,`3B-A`=0,`HR-A`=0,`P-OAB`=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D2 = D2 %>% mutate(`P-OAB` = as.numeric(`P-OAB`))
#Calculate XBH per BIP
D2 = D2 %>% mutate(`XBH_BIP` = ###WATCH YOUTUBE VIDEO FOR CODE###,
                   `XBH_BIP` =  round(`XBH_BIP`,3))

#D2 - Coe College, ncaa_scrape function allows us to get pitching data
D3 =  ncaa_scrape(150,2019,type="pitching")
#At least 1 apperance on the season
D3 = D3 %>% filter(App > 0)
#Replace NAs with 0 in specific columns
D3 = D3 %>% tidyr::replace_na(list(`2B-A`=0,`3B-A`=0,`HR-A`=0,`P-OAB`=0,SO=0))
#Make sure that P-OAB is numeric for calculation
D3 = D3 %>% mutate(`P-OAB` = as.numeric(`P-OAB`))
#Calculate XBH per BIP
D3 = D3 %>% mutate(`XBH_BIP` = ###WATCH YOUTUBE VIDEO FOR CODE###,
                   `XBH_BIP` =  round(`XBH_BIP`,3))

#View the metrics
D1 %>% select(Player,school,`P-OAB`,SO,XBH_BIP) %>% mutate(BIP = `P-OAB`-SO) %>% arrange(XBH_BIP)
D2 %>% select(Player,school,`P-OAB`,SO,XBH_BIP) %>% mutate(BIP = `P-OAB`-SO) %>% arrange(XBH_BIP)
D3 %>% select(Player,school,`P-OAB`,SO,XBH_BIP) %>% mutate(BIP = `P-OAB`-SO) %>% arrange(XBH_BIP)

#League Averages in 2019
#D1 XBH/BIP in 2019 = 10.3%
#D2 XBH/BIP in 2019 = 10.4%
#D3 XBH/BIP in 2019 = 9.1%
