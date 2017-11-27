# ===================================================================
# Title: Cleaning Data  ( inside the code/ folder)
# Description:
#   This script performs cleaning tasks and transformations on 
#   various columns of the raw data file.
# Input(s): data file 'rawscores.csv'
# Output(s): data file 'clean-data-script.csv'
# Author: Meiying Huang
# Date: 11-24-2017
# ===================================================================

# packages
library(readr)    # importing data
library(dplyr)    # data wrangling
library(ggplot2)  # graphics

#invoking source() to source in the functions in the script functions.R.
# source('../functions.R')

#read in the CSV file with the raw scores
raw_scores <- read.csv('../../rawdata/rawscores.csv', stringsAsFactors = FALSE)

# sink() the structure str() of the data frame of raw scores to a file summary-rawscores.txt
# since i have trouble input the source "functions.R, I put the summary_stat function and print_stats function here
summary_stats<-function(x, na.rm = TRUE, warning = FALSE){
  x<-list(
    min= get_min(x),
    percentile10= get_percentile10(x),
    quartile1= get_quartile1(x),
    med= get_med(x),
    avg= get_avg(x),
    quartile3= get_quartile3(x),
    percentile90= get_percentile90(x),
    max= get_max(x),
    range= get_range(x),
    sd= get_sd(x),
    missing= count_missing(x))
  return(x)
}

sumryhw1<-summary_stats(raw_scores$HW1)
sumryhw2<-summary_stats(raw_scores$HW2)
sumryhw3<-summary_stats(raw_scores$HW3)
sumryhw4<-summary_stats(raw_scores$HW4)
sumryhw5<-summary_stats(raw_scores$HW5)
sumryhw6<-summary_stats(raw_scores$HW6)
sumryhw7<-summary_stats(raw_scores$HW7)
sumryhw8<-summary_stats(raw_scores$HW8)
sumryhw9<-summary_stats(raw_scores$HW9)
sumryATT<-summary_stats(raw_scores$ATT)
sumryQZ1<-summary_stats(raw_scores$QZ1)
sumryQZ2<-summary_stats(raw_scores$QZ2)
sumryQZ3<-summary_stats(raw_scores$QZ3)
sumryQZ4<-summary_stats(raw_scores$QZ4)
sumryEX1<-summary_stats(raw_scores$EX1)
sumryEX2<-summary_stats(raw_scores$EX2)

sumry_raw_data<- data_frame(
  x_name = names(summary_stats(x)),
  summary_hw1= sumryhw1,
  summary_hw2= sumryhw2,
  summary_hw3= sumryhw3,
  summary_hw4= sumryhw4,
  summary_hw5= sumryhw5,
  summary_hw6= sumryhw6,
  summary_hw7= sumryhw7,
  summary_hw8= sumryhw8,
  summary_hw9= sumryhw9,
  summary_ATT= sumryATT,
  summary_QZ1= sumryQZ1,
  summary_QZ2= sumryQZ2,
  summary_QZ3= sumryQZ3,
  summary_QZ4= sumryQZ4,
  summary_EX1= sumryEX1,
  summary_EX2= sumryEX2)
str(sumry_raw_data)

#print_stats function
print_stats<- function(x, na.rm = TRUE, warning = FALSE){
  for(i in 1:length(x)){
    #name<- format(names(x[i]), width = max(nchar(names(x[i]))))
    x_name <- names(x)
    x1<- as.numeric(x[i])
    x2<- format(round(x1,digits = 4),nsmall = 4)
    y<- paste(x_name[i], ":", x2, sep = " ")
    cat(y, '\n')
  }
}

x<- sumry_raw_data
# print_stats(x)

# printhw1<-print_stats(raw_scores$HW1)
# printhw2<-print_stats(raw_scores$HW2)
# printhw3<-print_stats(raw_scores$HW3)
# printhw4<-print_stats(raw_scores$HW4)
# printhw5<-print_stats(raw_scores$HW5)
# printhw6<-print_stats(raw_scores$HW6)
# printhw7<-print_stats(raw_scores$HW7)
# printhw8<-print_stats(raw_scores$HW8)
# printhw9<-print_stats(raw_scores$HW9)
# printATT<-print_stats(raw_scores$ATT)
# printQZ1<-print_stats(raw_scores$QZ1)
# printQZ2<-print_stats(raw_scores$QZ2)
# printQZ3<-print_stats(raw_scores$QZ3)
# printQZ4<-print_stats(raw_scores$QZ4)
# printEX1<-print_stats(raw_scores$EX1)
# printEX2<-print_stats(raw_scores$EX2)


# print_raw_data<- data_frame(
# print_hw1= printhw1,
# print_hw2= printhw2,
# print_hw3= printhw3,
# print_hw4= printhw4,
# print_hw5= printhw5,
# print_hw6= printhw6,
# print_hw7= printhw7,
# print_hw8= printhw8,
# print_hw9= printhw9,
# print_ATT= printATT,
# print_QZ1= printQZ1,
# print_QZ2= printQZ2,
# print_QZ3= printQZ3,
# print_QZ4= printQZ4,
# print_EX1= printEX1,
# print_EX2= printEX2)

# str(print_raw_data)
raw_data2<- data_frame() 
 for(i in sumry_raw_data) {
   if(i=="NA"){
     print(i)= "0"
   }
 }
View(raw_data2)

str(raw_scores)
sink(file = 'summary-rawscores.txt')

# divert output to the specified file
sink(file = 'summary-height-weight.txt')
summary(dat[ ,c('height', 'weight')])
sink()



rescale100<- function(x, xmin, xmax){
  z<- 100*(x - xmin)/(xmax-xmin)
  return(z)
}  
b <- c(18, 15, 16, 4, 17, 9)
rescale100(b, xmin = 0, xmax = 20)

rescale100(raw_scores$QZ1,xmin = 0, xmax = 12)
rescale100(raw_scores$QZ2,xmin = 0, xmax = 18)
rescale100(raw_scores$QZ3,xmin = 0, xmax = 20)
rescale100(raw_scores$QZ4,xmin = 0, xmax = 20)

Test1<- rescale100(raw_scores$EX1,xmin = 0, xmax = 80)
Test2<- rescale100(raw_scores$EX2,xmin = 0, xmax = 90)

hw_fun<- function(x, shall_i_drop){
  if(shall_i_drop == TRUE){
    x <- drop_lowest(x) }
  n<- 0 
  for(i in 1:length(x)){
    n<- n + x[i]
  }
  avg<- n/length(x)
  return(avg)
}

x1<- c(raw_scores$HW1,raw_scores$HW2,raw_scores$HW3,raw_scores$HW4,raw_scores$HW5,raw_scores$HW6,raw_scores$HW7,raw_scores$HW8,raw_scores$HW9)
print(x)
Homwwork<- hw_fun(x1, shall_i_drop = TRUE)


x2<- c(raw_scores$HW1,raw_scores$HW2,raw_scores$HW3,raw_scores$HW4,raw_scores$HW5,raw_scores$HW6,raw_scores$HW7,raw_scores$HW8,raw_scores$HW9)
print(x)
Homwwork<- hw_fun(x1, shall_i_drop = TRUE)

summary_stats(raw_scores[ ,c('HW1', 'HW2','HW3','HW4','HW5','HW6','HW7','HW8','HW9',
                      'ATT', 'QZ1','QZ2','QZ3','QZ4', 'EX1', 'EX2')])

summary_stats(raw_scores[ ,c(1:16)])
sink()
#,'HW1', HW2','HW3','HW4','HW5','HW6','HW7','HW8','HW9','ATT', 'QZ1','QZ2','QZ3','QZ4', 'EX1', 'EX2')])
sink()
sink('../output/summary-rawscores.txt')

# Include also the summary statistics, using summary_stats() and print_stats(), for all the columns in the data frame.
#summary_stats(raw_scores$HW1)
# print(summary_stats(x))

# summary_stats(raw_scores[ ,c('HW1','HW2','HW3','HW4','HW5','HW6','HW7','HW8','HW9',
                       'ATT', 'QZ1','QZ2','QZ3','QZ4', 'EX1', 'EX2')])
