# -*- coding: utf-8 -*-
"""
@author: @RobertFrey40
"""
import pandas as pd
from datetime import datetime, timedelta
import sqlite3

#initial connection to statcast DB
conn = sqlite3.connect('minor_league_statcast.db')
c = conn.cursor()

#acquire opening day data for testing
od = datetime(2024, 3, 29)
od_str = od.strftime('%Y-%m-%d')
df_opening_day = statcast_minor_leagues(start_dt=od_str,end_dt=od_str)
df_opening_day.drop(columns=df_opening_day.columns[0], axis=1,  inplace=True)

#get opening day to current day
current_date = od
today = datetime.today()

#Loop through each day to acquire minor league statcast data
while current_date <= datetime(2024,4,2):
    date_str = current_date.strftime('%Y-%m-%d')
    print(f"Fetching minor league statcast data for {date_str}")
    
    df = statcast_minor_leagues(date_str)
    #Remove the 'index' column (optional)
    df.drop(columns=df.columns[0], axis=1,  inplace=True)
    #Rename some columns for easier handling with data
    df.rename({'pitcher.1': 'pitcher_1', 'fielder_2.1': 'fielder_2_1'}, axis=1, inplace=True)

    
    if df is not None and not df.empty:
        # Insert data into the SQLite database
        df.to_sql('statcast_data', conn, if_exists='append', index=False)
    
    # Move to the next date
    current_date += timedelta(days=1)

#Close the database connection
conn.close()

#Run a test query to make sure the data is uploaded
conn = sqlite3.connect('minor_league_statcast.db')

df = pd.read_sql('SELECT * FROM statcast_data WHERE release_speed >= 98', conn)

conn.close()


