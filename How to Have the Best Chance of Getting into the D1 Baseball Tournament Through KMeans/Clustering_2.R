library(tidyverse)
#install.packages('cluster')
library(cluster)    
#install.packages('factoextra')
library(factoextra) 

#Read in sample dataset I gave you, AGAIN NOT ALL OBSERVATIONS ARE PRESENT, SO FINAL NUMBERS WILL LOOK A LITTLE DIFFERENT
dataset = read.csv("sample_D1_team_data.csv", stringsAsFactors = F)

#Filter 2 dataframes, one for final output and one for the kmeans clustering
D1Data = dataset %>% select(school,year,tourn_appearance,BA,OBP,SLG,OPS,ISO,HR.PA,`K-BB%`,ERA,FIP,BAA,OBPA,SLGA,OPSA,SO_BB_Ratio)

df = alldata2 %>% select(school,year,tourn_appearance,BA,OBP,SLG,OPS,ISO,HR.PA,`K-BB%`,ERA,FIP,BAA,OBPA,SLGA,OPSA,SO_BB_Ratio)

#create a new column that combines school and year and add it as the rownames
df$schoolyear =paste0(df$school,"-",df$year)

rownames(df) <- df[,"schoolyear"]

df = df %>% select(-school,-year,-schoolyear)

#Scale data so that no observation greatly influences the outcome
df = scale(df)

#set seed to avoid repeat sampling
set.seed(1102)

#using kmeans, centers are the amount of centroids, nstart is the number of initial centroids,
#a higher number is good, like 25, since the dataset is not so large
k2 = kmeans(df,centers = 2, nstart = 25)
#visualize the cluster algorithm
fviz_cluster(k2, data = df)

k3 = kmeans(df,centers = 3, nstart = 25)
fviz_cluster(k3, data = df)

k4 = kmeans(df,centers = 4, nstart = 25)
fviz_cluster(k4, data = df)

k5 = kmeans(df,centers = 5, nstart = 25)
fviz_cluster(k5, data = df)

#Function to find the optimal number of k, where the plot starts to bend, in my case, it was 4.
#WATCH YOUTUBE VIDEO FOR CODE#

#Create a new column on the other dataset we created by using the optimal number of k's cluster and
#create a new variable called 'Cluster', group by Cluster, then summarise the statistics using the mean
#Each cluster tells you the average of each statistic and the average amount of tournament appearances
D1Data %>% ####WATCH YOUTUBE VIDEO FOR CODE####

#This time, no grouping and selecting a single team to determine what cluster they fit in
D1Data = D1Data %>% mutate(Cluster = k4$cluster)

D1Data %>% filter(school == "Virginia", year == 2014)

