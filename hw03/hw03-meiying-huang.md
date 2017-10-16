hw03-Meiying-Huang-rmd
================
Meiying Huang
10/12/2017

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
dat1 <- read.csv('./nba2017-roster.csv', stringsAsFactors = FALSE)
dat2<- read.csv('./nba2017-stats.csv', stringsAsFactors = FALSE)
```

``` r
##adding new variables
dat2$missed_fg=dat2$field_goals_atts-dat2$field_goals_made
dat2$missed_ft=dat2$points1_atts-dat2$points1_made
dat2$points=3*dat2$points3_made+2*dat2$points2_made+dat2$points1_made
dat2$rebounds=dat2$off_rebounds+dat2$def_rebounds
dat2$efficiency=((dat2$points+dat2$rebounds+dat2$assists+dat2$steals+dat2$blocks-dat2$missed_fg-dat2$missed_ft-dat2$turnover)/(dat2$games_played))

dat3<-cbind(player=dat2$player,dat2$missed_fg,dat2$missed_ft,dat2$points,dat2$rebounds,dat2$efficiency)

sink(file= '/Users/liminhuang/hw03/output/efficiency-summary.txt')
## summary(efficiency)
sink()
```

``` r
##merging tables
dat4<-merge(dat1,dat2)
```

``` r
##ceating nba2017-teams.csv
# dat4$team <- as.factor(dat$team)
teams <- dat4 %>% 
  group_by(team) %>%
  summarise(experience = sum(experience),
                  salary=round(sum(salary/1000000), digits=2),
                  points3=sum(points3_made),
                  points2=sum(points2_made),
                  free_throws=sum(points1_made),
                  points=sum(3*points3_made+2*points2_made+points1_made),
                  off_rebounds=sum(off_rebounds),
                  def_rebounds=sum(def_rebounds),
                  assists=sum(assists),
                  steals=sum(steals),
                  blocks=sum(blocks),
                  turnovers=sum(turnovers),
                  fouls=sum(fouls),
                  efficiency=sum(efficiency))
summary(teams)
```

    ##      team             experience         salary          points3      
    ##  Length:30          Min.   : 34.00   Min.   : 55.78   Min.   : 513.0  
    ##  Class :character   1st Qu.: 56.00   1st Qu.: 84.59   1st Qu.: 617.0  
    ##  Mode  :character   Median : 63.00   Median : 91.41   Median : 704.0  
    ##                     Mean   : 68.73   Mean   : 90.95   Mean   : 730.7  
    ##                     3rd Qu.: 73.25   3rd Qu.:101.87   3rd Qu.: 805.8  
    ##                     Max.   :128.00   Max.   :125.79   Max.   :1141.0  
    ##     points2      free_throws       points      off_rebounds  
    ##  Min.   :1769   Min.   : 998   Min.   :6348   Min.   :524.0  
    ##  1st Qu.:2115   1st Qu.:1238   1st Qu.:7561   1st Qu.:699.2  
    ##  Median :2252   Median :1384   Median :8164   Median :762.5  
    ##  Mean   :2242   Mean   :1359   Mean   :8035   Mean   :768.7  
    ##  3rd Qu.:2413   3rd Qu.:1492   3rd Qu.:8452   3rd Qu.:865.8  
    ##  Max.   :2638   Max.   :1605   Max.   :9473   Max.   :961.0  
    ##   def_rebounds     assists         steals          blocks     
    ##  Min.   :1878   Min.   :1291   Min.   :475.0   Min.   :234.0  
    ##  1st Qu.:2435   1st Qu.:1546   1st Qu.:544.8   1st Qu.:311.0  
    ##  Median :2536   Median :1738   Median :590.0   Median :351.5  
    ##  Mean   :2524   Mean   :1732   Mean   :583.3   Mean   :360.3  
    ##  3rd Qu.:2644   3rd Qu.:1858   3rd Qu.:612.0   3rd Qu.:389.5  
    ##  Max.   :2854   Max.   :2475   Max.   :779.0   Max.   :551.0  
    ##    turnovers          fouls        efficiency   
    ##  Min.   : 703.0   Min.   :1164   Min.   :125.1  
    ##  1st Qu.: 973.5   1st Qu.:1355   1st Qu.:143.8  
    ##  Median :1021.5   Median :1519   Median :146.7  
    ##  Mean   :1013.5   Mean   :1496   Mean   :149.0  
    ##  3rd Qu.:1087.2   3rd Qu.:1599   3rd Qu.:152.9  
    ##  Max.   :1184.0   Max.   :1886   Max.   :177.9

``` r
class(dat4$team)
```

    ## [1] "character"

``` r
unique(dat4$team)
```

    ##  [1] "DAL" "IND" "ORL" "MIN" "BOS" "POR" "LAC" "PHO" "UTA" "OKC" "PHI"
    ## [12] "NOP" "DET" "GSW" "MEM" "BRK" "CHI" "SAC" "HOU" "WAS" "LAL" "CHO"
    ## [23] "TOR" "SAS" "NYK" "CLE" "DEN" "ATL" "MIA" "MIL"

``` r
##sink team summary
sink(file= '/Users/liminhuang/hw03/data/teams-summary.txt')
summary(teams)
```

    ##      team             experience         salary          points3      
    ##  Length:30          Min.   : 34.00   Min.   : 55.78   Min.   : 513.0  
    ##  Class :character   1st Qu.: 56.00   1st Qu.: 84.59   1st Qu.: 617.0  
    ##  Mode  :character   Median : 63.00   Median : 91.41   Median : 704.0  
    ##                     Mean   : 68.73   Mean   : 90.95   Mean   : 730.7  
    ##                     3rd Qu.: 73.25   3rd Qu.:101.87   3rd Qu.: 805.8  
    ##                     Max.   :128.00   Max.   :125.79   Max.   :1141.0  
    ##     points2      free_throws       points      off_rebounds  
    ##  Min.   :1769   Min.   : 998   Min.   :6348   Min.   :524.0  
    ##  1st Qu.:2115   1st Qu.:1238   1st Qu.:7561   1st Qu.:699.2  
    ##  Median :2252   Median :1384   Median :8164   Median :762.5  
    ##  Mean   :2242   Mean   :1359   Mean   :8035   Mean   :768.7  
    ##  3rd Qu.:2413   3rd Qu.:1492   3rd Qu.:8452   3rd Qu.:865.8  
    ##  Max.   :2638   Max.   :1605   Max.   :9473   Max.   :961.0  
    ##   def_rebounds     assists         steals          blocks     
    ##  Min.   :1878   Min.   :1291   Min.   :475.0   Min.   :234.0  
    ##  1st Qu.:2435   1st Qu.:1546   1st Qu.:544.8   1st Qu.:311.0  
    ##  Median :2536   Median :1738   Median :590.0   Median :351.5  
    ##  Mean   :2524   Mean   :1732   Mean   :583.3   Mean   :360.3  
    ##  3rd Qu.:2644   3rd Qu.:1858   3rd Qu.:612.0   3rd Qu.:389.5  
    ##  Max.   :2854   Max.   :2475   Max.   :779.0   Max.   :551.0  
    ##    turnovers          fouls        efficiency   
    ##  Min.   : 703.0   Min.   :1164   Min.   :125.1  
    ##  1st Qu.: 973.5   1st Qu.:1355   1st Qu.:143.8  
    ##  Median :1021.5   Median :1519   Median :146.7  
    ##  Mean   :1013.5   Mean   :1496   Mean   :149.0  
    ##  3rd Qu.:1087.2   3rd Qu.:1599   3rd Qu.:152.9  
    ##  Max.   :1184.0   Max.   :1886   Max.   :177.9

``` r
sink()
```

``` r
##export teams to a csv named nba2017-teams.csv
write.csv(teams,file="/Users/liminhuang/hw03/data/nba2017-teams.csv",row.names=FALSE)
```

``` r
##graphics named: teams_stat_plot.pdf
stars(teams[,-1],labels = teams$team)
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
pdf(file="/Users/liminhuang/hw03/images/teams_stat_plot.pdf", width=6.5, height = 5)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
##a scatterplot of experience,salary,teams, save the plot in the file "experience_salary.pdf" inside the images folder
ggplot(data=teams, aes(x=experience,y=salary))+
  geom_label(aes(label= team), alpha=0.8,hjust=1,vjust=1)+
  ggtitle("Experience and Salary")
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
pdf(file = "/Users/liminhuang/hw03/images/experience-salary.pdf")
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
##1ranking: rank the teams according to salary,in decreasing order.ggplot to create a barchat(horizontally oriented), add a vertical red line - the average team salary
teams<-arrange(teams,desc(salary))
ggplot(teams,aes(x=reorder(team,salary),y=salary))+
  geom_bar(stat='identity',color="black",fill="gray")+
  ggtitle("NBA Teams ranked by Total Salary")+
  xlab("Team")+ylab("Salary(in millions)")+
  coord_flip()+
  geom_hline(yintercept=mean(teams$salary),color="red") ##cuz vertical, vline; if horizontal, then "hline"
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
##2nd ranking: bar chart of teams ranked by total points. the vertical line is the average team points
teams<-arrange(teams,points)
ggplot(teams,aes(x=reorder(team, points),y=points))+
  geom_bar(stat='identity',color="black",fill="gray")+
  ggtitle("NBA Teams ranked by Total Points")+
  xlab("Team")+ylab("Total Points")+
  coord_flip() +
  geom_hline(yintercept=mean(teams$points),color="red")
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
##3rd ranking, ranked by total efficiency
teams<-arrange(teams,efficiency)
ggplot(teams,aes(x=reorder(team, efficiency) ,y=efficiency))+
  geom_bar(stat='identity',color="black",fill="gray")+
  ggtitle("NBA Teams ranked by Total Efficiency")+
  xlab("Team")+ylab("Total Efficiency")+
  coord_flip() +
  geom_hline(yintercept=mean(teams$efficiency),color="red")
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
##comment of rankings
##ranking#1:From the "NBA Teams ranked by Total Salary" ranking, we can see that CLE is the team that has the highest salary, and PHI team has the lowest salary. From the red line that indicates the average salary, we know that the average sarlasy is around 90 missions.

##ranking#2:From the "NBA Teams ranked by Total Points" ranking, we can see that GSW is ranked at top 1 and SAC has the least total points. The average total point for team is around 8000.

##ranking#3: From the "NBA Teams ranked by Total Efficiency" ranking, we can see that CLE is the most efficient team, and ORL has the least efficiency. And the average efficiency among thes NBA teams is around 150
```

``` r
##PCA:principal components Analysis
#to get a data frame with the eigenvalues
pca1<-select(teams,points3,points2,free_throws,off_rebounds,def_rebounds,assists,steals,blocks,turnovers,fouls)
pca<-prcomp(pca1,scale. = TRUE)##is "true" indicates pca is performed on standardized data(mean=0,var=1)
pca
```

    ## Standard deviations:
    ##  [1] 2.1669994 1.3046111 0.9897094 0.8784756 0.7308134 0.6913872 0.6182263
    ##  [8] 0.5101592 0.3655034 0.2503921
    ## 
    ## Rotation:
    ##                    PC1         PC2         PC3          PC4         PC5
    ## points3      0.1121782 -0.65652993  0.28806873 -0.042637313  0.28657624
    ## points2      0.3601766  0.32892544 -0.06763180 -0.347710703 -0.15173866
    ## free_throws  0.3227564 -0.17651228  0.39157491  0.147596178 -0.21363792
    ## off_rebounds 0.3029366  0.35931603  0.33884845 -0.288483019 -0.16571824
    ## def_rebounds 0.3719432 -0.12808273  0.15026131 -0.492969442  0.26476256
    ## assists      0.3125312 -0.44134618 -0.26294129 -0.088066602 -0.36972525
    ## steals       0.3447256 -0.03540585 -0.48554101  0.177578661 -0.33549491
    ## blocks       0.3162237  0.06131890 -0.48869371  0.003935374  0.65459381
    ## turnovers    0.3353958 -0.02169833  0.08910421  0.532117541 -0.04471763
    ## fouls        0.3072548  0.28954426  0.26469871  0.454751471  0.26814214
    ##                       PC6         PC7         PC8         PC9         PC10
    ## points3      -0.028435666  0.38167878 -0.18027569  0.20631322  0.409762462
    ## points2      -0.088714347  0.07302430  0.47216199  0.35836740  0.499011524
    ## free_throws  -0.487342521 -0.62732220 -0.07726675  0.08283563 -0.006875686
    ## off_rebounds  0.283093235  0.13535335 -0.64646479  0.14735551 -0.124601143
    ## def_rebounds  0.066309015 -0.04926346  0.23787252 -0.64632050 -0.168579984
    ## assists       0.176019008  0.11785039  0.18235775  0.34086739 -0.547385461
    ## steals       -0.303664534  0.25883825 -0.32703573 -0.41596580  0.246739300
    ## blocks       -0.009954065 -0.30799231 -0.23947533  0.27071160 -0.057627209
    ## turnovers     0.675777660 -0.18850849  0.14308362 -0.13524769  0.250947823
    ## fouls        -0.298848473  0.47268121  0.21462859  0.04367200 -0.335087245

``` r
names
```

    ## function (x)  .Primitive("names")

``` r
##creat a data frame with eigenvalues

eigs<-data.frame(team=teams$team,
                   eigenvalue=pca$sdev^2,
                 proportion=round(pca$sdev^2/sum(pca$sdev^2)
                                  ,3),
                 cumprop=cumsum(round(pca$sdev^2/sum(pca$sdev^2)
                                  ,3)),
                 PC1=round(pca$rotation[,1],3),##weight of PC1
                 PC2=round(pca$rotation[,2],3)) ##weight of PC2
```

    ## Warning in data.frame(team = teams$team, eigenvalue = pca$sdev^2,
    ## proportion = round(pca$sdev^2/sum(pca$sdev^2), : row names were found from
    ## a short variable and have been discarded

``` r
eigs                
```

    ##    team eigenvalue proportion cumprop   PC1    PC2
    ## 1   ORL 4.69588631      0.470   0.470 0.112 -0.657
    ## 2   IND 1.70201009      0.170   0.640 0.360  0.329
    ## 3   DET 0.97952464      0.098   0.738 0.323 -0.177
    ## 4   CHI 0.77171938      0.077   0.815 0.303  0.359
    ## 5   ATL 0.53408824      0.053   0.868 0.372 -0.128
    ## 6   MEM 0.47801622      0.048   0.916 0.313 -0.441
    ## 7   WAS 0.38220374      0.038   0.954 0.345 -0.035
    ## 8   POR 0.26026243      0.026   0.980 0.316  0.061
    ## 9   NYK 0.13359274      0.013   0.993 0.335 -0.022
    ## 10  LAL 0.06269622      0.006   0.999 0.307  0.290
    ## 11  PHO 4.69588631      0.470   0.470 0.112 -0.657
    ## 12  MIN 1.70201009      0.170   0.640 0.360  0.329
    ## 13  CHO 0.97952464      0.098   0.738 0.323 -0.177
    ## 14  UTA 0.77171938      0.077   0.815 0.303  0.359
    ## 15  SAS 0.53408824      0.053   0.868 0.372 -0.128
    ## 16  OKC 0.47801622      0.048   0.916 0.313 -0.441
    ## 17  LAC 0.38220374      0.038   0.954 0.345 -0.035
    ## 18  BRK 0.26026243      0.026   0.980 0.316  0.061
    ## 19  DAL 0.13359274      0.013   0.993 0.335 -0.022
    ## 20  BOS 0.06269622      0.006   0.999 0.307  0.290
    ## 21  SAC 4.69588631      0.470   0.470 0.112 -0.657
    ## 22  MIA 1.70201009      0.170   0.640 0.360  0.329
    ## 23  MIL 0.97952464      0.098   0.738 0.323 -0.177
    ## 24  HOU 0.77171938      0.077   0.815 0.303  0.359
    ## 25  TOR 0.53408824      0.053   0.868 0.372 -0.128
    ## 26  PHI 0.47801622      0.048   0.916 0.313 -0.441
    ## 27  NOP 0.38220374      0.038   0.954 0.345 -0.035
    ## 28  DEN 0.26026243      0.026   0.980 0.316  0.061
    ## 29  GSW 0.13359274      0.013   0.993 0.335 -0.022
    ## 30  CLE 0.06269622      0.006   0.999 0.307  0.290

``` r
##Use PC1 PC2 to get a data frame before getting a scatterplot of the teams
pc_teams<-data.frame(team=teams$team,PC1=round(pca$rotation[,1],3),
                 PC2=round(pca$rotation[,2],3))
```

    ## Warning in data.frame(team = teams$team, PC1 = round(pca$rotation[, 1], :
    ## row names were found from a short variable and have been discarded

``` r
pc_teams
```

    ##    team   PC1    PC2
    ## 1   ORL 0.112 -0.657
    ## 2   IND 0.360  0.329
    ## 3   DET 0.323 -0.177
    ## 4   CHI 0.303  0.359
    ## 5   ATL 0.372 -0.128
    ## 6   MEM 0.313 -0.441
    ## 7   WAS 0.345 -0.035
    ## 8   POR 0.316  0.061
    ## 9   NYK 0.335 -0.022
    ## 10  LAL 0.307  0.290
    ## 11  PHO 0.112 -0.657
    ## 12  MIN 0.360  0.329
    ## 13  CHO 0.323 -0.177
    ## 14  UTA 0.303  0.359
    ## 15  SAS 0.372 -0.128
    ## 16  OKC 0.313 -0.441
    ## 17  LAC 0.345 -0.035
    ## 18  BRK 0.316  0.061
    ## 19  DAL 0.335 -0.022
    ## 20  BOS 0.307  0.290
    ## 21  SAC 0.112 -0.657
    ## 22  MIA 0.360  0.329
    ## 23  MIL 0.323 -0.177
    ## 24  HOU 0.303  0.359
    ## 25  TOR 0.372 -0.128
    ## 26  PHI 0.313 -0.441
    ## 27  NOP 0.345 -0.035
    ## 28  DEN 0.316  0.061
    ## 29  GSW 0.335 -0.022
    ## 30  CLE 0.307  0.290

``` r
ggplot(data=pc_teams, aes(x=PC1,y=PC2),label=team)+
  geom_label(aes(label= team), alpha=0.5,hjust=1,vjust=1)
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
##scores
round(head(pca$x,13),1)
```

    ##        PC1  PC2  PC3  PC4  PC5  PC6  PC7  PC8  PC9 PC10
    ##  [1,] -1.5  0.2 -0.2 -0.1 -0.5  0.4  0.1  0.6 -0.1 -0.4
    ##  [2,] -0.1  0.4 -1.0  0.3  0.0 -0.4 -0.2  0.4  0.1  0.2
    ##  [3,]  0.4  1.4  0.1 -2.3 -0.4  0.5  1.0  0.7 -0.5  0.1
    ##  [4,] -0.8  0.5 -0.6 -0.7 -1.2  0.3 -0.5 -1.0 -0.1 -0.2
    ##  [5,]  0.3  0.1 -0.4  0.1 -0.8  1.1 -0.8 -0.1 -0.4  0.3
    ##  [6,]  0.6  0.5  1.0  0.6 -0.1 -0.9  0.7 -0.6 -0.3 -0.6
    ##  [7,] -0.1  0.2 -0.4  0.0 -1.1  0.0  0.3  0.5  0.4  0.2
    ##  [8,] -0.9 -0.2  1.0  0.5  0.9 -0.4  0.1  0.2  0.3  0.3
    ##  [9,]  0.5  2.0  0.3 -1.0  1.4  0.4 -0.1 -0.5  0.7 -0.1
    ## [10,] -0.7  2.0  0.2  0.1 -0.1  0.8  1.2 -0.5 -0.7  0.2
    ## [11,]  2.0  2.3  1.2  1.6  0.0 -0.5 -0.4  0.4  0.4  0.2
    ## [12,]  2.6  1.2  0.1  0.2 -1.4 -0.3 -0.2 -0.1  0.5  0.0
    ## [13,] -1.4 -1.8  0.1 -1.5 -0.3 -1.0 -1.0  0.2 -0.2 -0.1

``` r
round(head(pca$x,13),2)
```

    ##         PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8   PC9  PC10
    ##  [1,] -1.48  0.22 -0.16 -0.07 -0.47  0.35  0.08  0.63 -0.06 -0.41
    ##  [2,] -0.14  0.36 -1.03  0.29 -0.02 -0.44 -0.19  0.36  0.07  0.22
    ##  [3,]  0.42  1.35  0.14 -2.30 -0.40  0.55  1.01  0.69 -0.48  0.06
    ##  [4,] -0.79  0.55 -0.60 -0.68 -1.17  0.27 -0.52 -0.99 -0.11 -0.24
    ##  [5,]  0.29  0.13 -0.38  0.06 -0.82  1.05 -0.81 -0.15 -0.38  0.27
    ##  [6,]  0.61  0.47  1.02  0.58 -0.10 -0.92  0.75 -0.61 -0.29 -0.58
    ##  [7,] -0.11  0.21 -0.45  0.02 -1.12  0.05  0.35  0.51  0.37  0.24
    ##  [8,] -0.90 -0.21  1.04  0.48  0.90 -0.40  0.13  0.20  0.35  0.29
    ##  [9,]  0.48  2.03  0.32 -1.00  1.35  0.42 -0.08 -0.52  0.66 -0.12
    ## [10,] -0.74  2.02  0.24  0.06 -0.13  0.79  1.25 -0.47 -0.66  0.18
    ## [11,]  2.04  2.30  1.20  1.59  0.04 -0.47 -0.44  0.45  0.40  0.16
    ## [12,]  2.58  1.18  0.09  0.18 -1.40 -0.35 -0.21 -0.07  0.48 -0.04
    ## [13,] -1.42 -1.77  0.07 -1.46 -0.33 -1.00 -1.01  0.18 -0.22 -0.13

``` r
round(pca$rotation,2)
```

    ##               PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8   PC9  PC10
    ## points3      0.11 -0.66  0.29 -0.04  0.29 -0.03  0.38 -0.18  0.21  0.41
    ## points2      0.36  0.33 -0.07 -0.35 -0.15 -0.09  0.07  0.47  0.36  0.50
    ## free_throws  0.32 -0.18  0.39  0.15 -0.21 -0.49 -0.63 -0.08  0.08 -0.01
    ## off_rebounds 0.30  0.36  0.34 -0.29 -0.17  0.28  0.14 -0.65  0.15 -0.12
    ## def_rebounds 0.37 -0.13  0.15 -0.49  0.26  0.07 -0.05  0.24 -0.65 -0.17
    ## assists      0.31 -0.44 -0.26 -0.09 -0.37  0.18  0.12  0.18  0.34 -0.55
    ## steals       0.34 -0.04 -0.49  0.18 -0.34 -0.30  0.26 -0.33 -0.42  0.25
    ## blocks       0.32  0.06 -0.49  0.00  0.65 -0.01 -0.31 -0.24  0.27 -0.06
    ## turnovers    0.34 -0.02  0.09  0.53 -0.04  0.68 -0.19  0.14 -0.14  0.25
    ## fouls        0.31  0.29  0.26  0.45  0.27 -0.30  0.47  0.21  0.04 -0.34

``` r
##Use PC1 PC2 to get a scatterplot of the teams
PCA<-princomp(pca1,col=T)
```

    ## Warning: In princomp.default(pca1, col = T) :
    ##  extra argument 'col' will be disregarded

``` r
PCA
```

    ## Call:
    ## princomp(x = pca1, col = T)
    ## 
    ## Standard deviations:
    ##    Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7 
    ## 369.92849 235.83537 155.88724 133.63215 106.55001  84.50064  75.25505 
    ##    Comp.8    Comp.9   Comp.10 
    ##  53.13791  47.50244  27.63758 
    ## 
    ##  10  variables and  30 observations.

``` r
PCA$loadings
```

    ## 
    ## Loadings:
    ##              Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8
    ## points3       0.126  0.501 -0.322  0.261 -0.312  0.286 -0.379  0.252
    ## points2       0.514 -0.500  0.381                0.342 -0.134  0.384
    ## free_throws   0.313        -0.530         0.768                     
    ## off_rebounds  0.201 -0.243         0.120        -0.478 -0.702 -0.384
    ## def_rebounds  0.492                0.691 -0.137 -0.184  0.367       
    ## assists       0.465  0.568  0.387 -0.428                      -0.153
    ## steals        0.105               -0.182                      -0.417
    ## blocks        0.107                      -0.114         0.394 -0.379
    ## turnovers     0.190        -0.174 -0.332 -0.164 -0.672  0.178  0.512
    ## fouls         0.254 -0.322 -0.531 -0.326 -0.493  0.273        -0.172
    ##              Comp.9 Comp.10
    ## points3      -0.395  0.138 
    ## points2      -0.214        
    ## free_throws                
    ## off_rebounds               
    ## def_rebounds  0.281        
    ## assists       0.221 -0.215 
    ## steals       -0.110  0.869 
    ## blocks       -0.745 -0.340 
    ## turnovers    -0.146  0.170 
    ## fouls         0.270 -0.145 
    ## 
    ##                Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8
    ## SS loadings       1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0
    ## Proportion Var    0.1    0.1    0.1    0.1    0.1    0.1    0.1    0.1
    ## Cumulative Var    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8
    ##                Comp.9 Comp.10
    ## SS loadings       1.0     1.0
    ## Proportion Var    0.1     0.1
    ## Cumulative Var    0.9     1.0

``` r
loadings<-as.data.frame(PCA$loading[,1:2])
PCA2<-prcomp(pca1)
loadings<-as.data.frame(PCA2$rotation[,1:2])
PCA3<-prcomp(pca1)
loadings<-as.data.frame(PCA3$CA$v.eig[,1:2])

ggplot(data=pc_teams, aes(x=PC1,y=PC2),label=team)+
  geom_label(aes(label= team), alpha=0.5,hjust=1,vjust=1)
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
##ranking5: ranked by scaled PC1
teams2<-arrange(pc_teams,PC1)
ggplot(pc_teams,aes(x=reorder(team, PC1) ,y=PC1))+
  geom_bar(stat='identity',color="black",fill="gray")+
  ggtitle("NBA Teams ranked by scaled PC1")+
  xlab("Team")+ylab("First PC(scaled from 0to 100)")+
  coord_flip() 
```

![](hw03-meiying-huang-markdown_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
##scale:a brief description of PC1 index to rank the teams:the predicted coordinates of the new individuals can be manually calculated as follow:
##1) center and scale the new individual data using the center and the scale of the PCA
##2)calculate the predicted coordinates by mutiplying the scaled values with the eigenvectors(loadings) of the principal components

eig.val=eigs$eigenvalue
eig.val
```

    ##  [1] 4.69588631 1.70201009 0.97952464 0.77171938 0.53408824 0.47801622
    ##  [7] 0.38220374 0.26026243 0.13359274 0.06269622 4.69588631 1.70201009
    ## [13] 0.97952464 0.77171938 0.53408824 0.47801622 0.38220374 0.26026243
    ## [19] 0.13359274 0.06269622 4.69588631 1.70201009 0.97952464 0.77171938
    ## [25] 0.53408824 0.47801622 0.38220374 0.26026243 0.13359274 0.06269622

``` r
s1_func<-function(z1,s1){
  s1=100*((z1-min(z1))/(max(z1)-min(z1)))
  apply(s,2,sum)
  }
  
##coordinates of the individividuals
s1_func<- function(z1,s1){
  s<-100*((z1-min(z1))/((max(z1)-min(z1))))
  apply(s,2,sum)
}
```

``` r
##comments and reflections
## yes. It was a fair peoject,not too hard, not too easy.But it took me longer than I think it should be
## It's my second time to use the related path. My GSI showed me once when i was working on the lab assigbment. I found it very useful. It's easy to place/pull out the files you need/store.
##second time to use R script. Still not really understanding the difference between markdown and script. Like, we need to use %>% in script but not in markdown. I am not sure what else code will change between this two.
##what things were hard? --I thik the PC part was the most hard one for me. I did this project before the lab session. And I have to google a lot to get it done.
##GOOGLE and YOUTUBE are my bestfriends !!
##what's easy? The barchat was okay.
##more time consuming part? --definitly the PC part. And the cvs part as well. I am not quite understand how to deal with the cvs
##How long it took me to finish this hw? --5hours
##interesting?--if fixing the bugs counts, then yes.  
```
