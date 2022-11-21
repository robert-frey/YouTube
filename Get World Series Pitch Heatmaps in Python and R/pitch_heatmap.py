########### BE SURE TO RUN THESE LINES OF CODE IN R BEFORE USING PYTHON #####
#install.packages('reticulate')
#library(reticulate)
#virtualenv_create('venv')
#repl_python()

##### BE SURE TO RUN '%pip install package_name' IF YOU DON'T HAVE THESE PACKAGES'
import baseball_scraper as bsb
import pandas as pd
import numpy as np
from plotnine import *

#acquires world series statcast data
world_series = bsb.statcast(start_dt="2022-10-29",end_dt="2022-11-05")

#lists out columns of world series data
list(world_series.columns)

#adds a new column to get pitcher team
world_series['pitch_team']=np.where(world_series['inning_topbot']=='Top',world_series['home_team'],world_series['away_team'])

#get average velocity by team and pitch team
world_series.groupby(['pitch_team','pitch_name']).agg({'release_speed':['mean']})

#create a function to get a heatmap of team and pitch type
def get_pitch_heatmap(df,team,pitch_type):
  
  if (pitch_type == 'Breaking Ball'):
    pitch_df=df.query(f"pitch_team== @team & pitch_name == 'Slider' | pitch_name == 'Curveball' | pitch_name == 'Knuckle Curve'")
  elif (pitch_type == 'Fastball'):
    pitch_df=df.query(f"pitch_team== @team & pitch_name == '4-Seam Fastball' | pitch_name == 'Sinker' | pitch_name == 'Cutter'")
  elif (pitch_type == 'Offspeed'):
    pitch_df=df.query(f"pitch_team== @team & pitch_name == 'Changeup' | pitch_name == 'Split-Finger'")
    
  
  plot=ggplot(pitch_df,aes(x="plate_x",y="plate_z", ymin = 0, ymax =5)) \
  + stat_density_2d(aes(fill='..density..'),geom='raster', contour = False) \
  + scale_fill_distiller(type="div") \
  + xlim(-2,2) \
  + annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1) \
  + theme_classic() \
  + xlab("Horizontal Pitch Location") \
  + ylab("Vertical Pitch Location") \
  + ggtitle(title=f"{team} {pitch_type} Pitch Heatmap (Catcher's View)")
  
  
  return(plot)

#Run the Function
get_pitch_heatmap(df=world_series,team="PHI",pitch_type="Fastball")
