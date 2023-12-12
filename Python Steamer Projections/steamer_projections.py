
"""
Created on Wed Dec  6 13:10:16 2023

@author: Robert Frey
@twitter/x: @RobertFrey40
@YouTube: youtube.com/@robertfrey40
"""

import json
import pandas as pd
import requests

def steamer_projections():
    #Steamer Projections URL
    fg_api = "https://www.fangraphs.com/api/projections?type=steamer&stats=bat&pos=all&team=0&players=0&lg=all"
    
    b = requests.get(fg_api)
    
    df = json.loads(b.content)
    
    df2 = pd.json_normalize(df)
    
    df2 = df2.rename(columns={"playerids":"player_id","PlayerName":"name","Team":"team_abbr","ShortName":"team_name",
                      "BB%":"BB_pct","K%":"K_pct"})

    df2 = df2[['player_id','name','team_abbr','team_name','G','PA','AVG','HR','RBI','OPS',
      'BB_pct','K_pct','wRC+','WAR']]
    
    df2['OPS'] = df2['OPS'].round(3)
    df2['AVG'] = df2['AVG'].round(3)
    df2['BB_pct'] = df2['BB_pct'].round(3)
    df2['K_pct'] = df2['K_pct'].round(3)
    df2['WAR'] = df2['WAR'].round(1)
    df2['wRC+'] = df2['wRC+'].round(0)  
    df2['player_id']=df2['player_id'].astype(str)
    
    #2023 Data URL
    fg_api = "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2023&season1=2023&startdate=2023-03-01&enddate=2023-11-01&month=0&hand=&team=0&pageitems=100000&pagenum=1&ind=0&rost=0&players=&type=8"
    
        
    b = requests.get(fg_api)
    
    #This slightly differs due to combination of JSON formats
    df = json.loads(b.content)['data']
    
    df3 = pd.json_normalize(df)
    
    df3 = df3.rename(columns={"playerid":"player_id","PlayerName":"name",
                              "TeamName":"team_abbr",
                      "BB%":"BB_pct_23","K%":"K_pct_23","G":"G_23","PA":"PA_23",
                      "HR":"HR_23",
                      "AVG":"AVG_23","RBI":"RBI_23",
                      "wRC+":"wRC+_23","WAR":"WAR_23","OPS":"OPS_23"})

    df3 = df3[['player_id','G_23','PA_23','AVG_23','HR_23','RBI_23','OPS_23',
      'BB_pct_23','K_pct_23','wRC+_23','WAR_23']]

    df3['OPS_23'] = df3['OPS_23'].round(3)
    df3['AVG_23'] = df3['AVG_23'].round(3)
    df3['BB_pct_23'] = df3['BB_pct_23'].round(3)
    df3['K_pct_23'] = df3['K_pct_23'].round(3)
    df3['WAR_23'] = df3['WAR_23'].round(1)
    df3['wRC+_23'] = df3['wRC+_23'].round(0)
    df3['player_id']=df3['player_id'].astype(str)
    
    projections = df2.merge(df3,on ='player_id')
    
    projections['wRC+_diff']=projections['wRC+']-projections['wRC+_23']
    projections['WAR_diff']=projections['WAR']-projections['WAR_23']
    
    projections = projections[projections['PA'] >= 200]
    
    return projections;
    
test = steamer_projections()