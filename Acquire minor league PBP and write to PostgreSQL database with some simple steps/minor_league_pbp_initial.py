import statsapi

#THIS IS NOT THE FINAL CODE - FINAL CODE WILL BE UPDATED WHEN YOUTUBE VIDEO IS PUBLISHED

# Acquire minor league play by play data
 sched_aaa = statsapi.schedule(start_date = '04/05/2022', end_date = '09/28/2022', sportId=11)
 sched_aa = statsapi.schedule(start_date = '04/08/2022', end_date = '09/18/2022', sportId=12)
 sched_hi_a = statsapi.schedule(start_date = '04/08/2022', end_date = '09/11/2022', sportId=13)
 sched_low_a = statsapi.schedule(start_date = '04/08/2022', end_date = '09/11/2022', sportId=14)
 sched_rok_dsl = statsapi.schedule(start_date = '06/06/2022', end_date = '08/23/2022', sportId=16)

# create list of game_pks
game_pks = []

# put game pks in list
for a in sched_aaa:
    game_pks.append(a['game_id'])

for b in sched_aa:
    game_pks.append(b['game_id'])

for c in sched_hi_a:
    game_pks.append(c['game_id'])

for d in sched_low_a:
    game_pks.append(d['game_id'])

for z in sched_rok_dsl:
    game_pks.append(z['game_id'])

#example to acquire pbp data
#Game ID is an April 24 matchup between the Myrtle Beach Pelicans and Delmarva Shorebirds
statsapi.get('game_playByPlay', params={'gamePk':669884},force=True)


