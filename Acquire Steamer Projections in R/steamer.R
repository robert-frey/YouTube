#load in libs
library(jsonlite)
library(dplyr)

#Source a function
source("https://raw.githubusercontent.com/robert-frey/HelperFiles/main/steamer_projections.R")

# Create Differences
steamer <- steamer %>% dplyr::mutate(
  G_diff = G - G_23,
  PA_diff = PA - PA_23,
  HR_diff = HR - HR_23,
  RBI_diff = RBI-RBI_23,
  OPS_diff = round(OPS-OPS_23,3),
  BB_pct_diff = round(BB_pct-BB_pct_23,3),
  K_pct_diff = round(K_pct-K_pct_23,3),
  wRC_diff = round(`wRC+`-`wRC+_23`),
  WAR_diff = round(WAR-WAR_23,1)
) %>%
  dplyr::filter(PA >= 200)

#install.packages('gt')
library(gt)
#install.packages('gtExtras')
library(gtExtras)

#Look at different Metrics
steamer %>% dplyr::filter(PA_23 >= 100, %>%
  arrange(-OPS_diff) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(name,team_name,PA,OPS,PA_23,OPS_23,OPS_diff) %>%
  gt() %>% gt_theme_espn() %>%
  tab_header(title = "Top 5 OPS Gainers from Steamer Projections",
             subtitle = "Min. 100 PAs in 2023")

steamer %>% dplyr::filter(PA_23 >= 100, OPS >= .800) %>%
  arrange(-OPS_diff) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(name,team_name,PA,OPS,PA_23,OPS_23,OPS_diff) %>%
  gt() %>% gt_theme_espn() %>%
  tab_header(title = "Top 5 OPS Gainers from Steamer Projections",
             subtitle = "Min. 100 PAs in 2023, Projected OPS of .800 or greater")
