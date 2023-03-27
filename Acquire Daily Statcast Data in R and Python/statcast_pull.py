# -*- coding: utf-8 -*-
"""
Created on Sun Mar 26 12:17:52 2023

@author: rfrey
"""
#pip install pandas
import pandas as pd
#pip install numpy
import numpy as np
#pip install datetime
import datetime
#pip install sqlite3
import sqlite3
#pip install baseball-scraper
from baseball_scraper import statcast

#Today's Date
today = datetime.date.today()

today_str = today.strftime('%Y-%m-%d')

prev_day = today - datetime.timedelta(days=1)
prev_day = prev_day.strftime('%Y-%m-%d')

#Acquire the previous day of data
data = statcast(start_dt = today_str, end_dt = prev_day)

#Show the columns
list(data.columns.values)

# Connect to DB and write to table
conn = sqlite3.connect('statcast_2023.db')

table_name = 'statcast_data_2023'

data.to_sql(table_name,conn,if_exists='append',index=False)

conn.close()

# Test to see data is there
#conn = sqlite3.connect('statcast_2023.db')

#cur = conn.cursor()

#df = cur.execute("SELECT * FROM statcast_data_2023 LIMIT 10")

#print(df.fetchone())

# conn = sqlite3.connect('statcast_2023.db')

# cur = conn.cursor()

# cur.execute("DELETE FROM statcast_data_2023")

# conn.commit()

# conn.close()
