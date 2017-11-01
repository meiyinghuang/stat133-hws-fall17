##title: post01
##description: The puroose of this project is to creat a new ranking system for NBA teams. identify the factors that are most likely to contribute to NBA player salaries. And to identify the performance variables i.e. scoring, assists, fouls, and other variables that significantly contributed to determine a NBA player's salary. 
##input: hw03(nba2017-roster.csv, nba2017-stats.csv)
##output:(clean-data.csv)
# Author: Meiying Huang
# Date: 10-31-2017

# packages
library(readr)    # importing data
library(dplyr)    # data wrangling
library(ggplot2)  # graphics

dat2<-read.csv("~/stat133/stat133-hws-fall17/stat133/stat133-hws-fall17/hw03/data/nba2017-stats.csv")
write.csv(dat2,
          file="/Users/liminhuang/post01/data/nba2017-ststa.csv",row.names=FALSE)

dat3<- read.csv("~/stat133/stat133-hws-fall17/stat133/stat133-hws-fall17/hw03/data/nba2017-roster.csv")
write.csv(dat3,
          file = "/Users/liminhuang/post01/data/nba2017-roster.csv",row.names = FALSE)


dat2%>%
  mutate(dat2, missed_fg="missed field goals", 
         miss_ft="missing free throws", 
         points="total points", 
         rebounds= "offensive rebounds+defensive rebounds",
         efficiency="efficiency index")
mutate(dat2,
       efficiency=
         (points+rebounds+assists+steals+blocks-missed_ft-turnovers)
       /games_played)

summary(dat2, efficiency)

sink(file = 'efficiency-summary.txt')
summary(dat2,efficiency)
sink()

##merging tables
dat4<- merge(dat2,dat3)%>%
  group_by(team)%>%

sink(file = 'team-summary.txt')
summary(dat2,team)
sink()
