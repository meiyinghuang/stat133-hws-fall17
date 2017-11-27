
#' @title remove_missing
#' @description remove the missing values form the vector
#' @param x numeric vector
#' @param na.rm whether to remove missing values
#' @return vector in standardized units
remove_missing<- function(x,na.rm = TRUE){
  stopifnot(is.numeric(x))
    warning("non-numeric argument")
      x<-x[!is.na(x)]
  return(x)
}

a <- c(1, 4, 7, NA, 10)
remove_missing(a)

#' @title get_minimum
#' @description compute the minimum value of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the minimum value of a vector
get_min<- function(x,na.rm = TRUE){
  stopifnot(is.numeric(x))
    warning("non-numeric argument")
  x<- sort(remove_missing(x))
  x<- x[1]
  return(x)
}

a <- c(1, 4, 7, NA, 10)
get_min(a)

#' @title get_maximum
#' @description compute the maximum value of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the maximum value of a vector
get_max<- function(x,na.rm = TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x),decreasing = TRUE)
  x<- x[1]
  return(x)
}

a <- c(1, 4, 7, NA, 10)
get_max(a)

#' @title get_range
#' @description compute the range of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the range of a vector
get_range<-function(x,na.rm=TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- remove_missing(x)
  get_range<- get_max(x) - get_min(x)
  return(get_range)
}

a <- c(1, 4, 7, NA, 10)
get_range(a)

#' @title get_median
#' @description compute the median of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the median of a vector
get_med<-function(x,na.rm=TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x))
  n<- length(x)
  if (n%%2!=0){
    m<-x[(n+1)/2]
  }else{
    u<-n/2 
    v<-u+1
    m<-(x[u]+x[v])/2
    get_med<-(m)
  }
return(get_med)
}

a <- c(1, 4, 7, NA, 10)
get_med(a,na.rm = TRUE)

#' @title get_average
#' @description compute the median of a vector
#' @param x data numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the median of a vector
get_avg<-function(x,na.rm=TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x))
  l<- length(x)
  n<-0
  for(i in 1:length(x)){
    n<- n+x[i]}
  get_avg<- n/l
  return(get_avg)
}

a <- c(1, 4, 7, NA, 10)
get_avg(a,na.rm = TRUE)

#' @title get_sd
#' @description convert x to standard units
#' @param x numeric vector
#' @param na.rm whether to remove missing values
#' @return vector in standardized units
#' standardize(1:5)
get_sd<- function(x, na.rm = TRUE) {
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x))
  n<- length(x)
  d<-0
  i<-1
  for(i in i:length(x)){
    x_bar <- get_avg(x, na.rm = TRUE)
    d<- d + (x[i] - x_bar)^2
  }
  sd<- sqrt(d/(n-1))
  return(sd)
}

a <- c(1, 4, 7, NA, 10)
get_sd(a,na.rm = TRUE)

#' @title get_quartile1
#' @description compute the 25th percentiles of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the 25th percentiles of a vector
get_quartile1<-function(x,na.rm=TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x))
  get_quartile1<- quantile(x, prob=c(0.25))
  return(get_quartile1)
}

a <- c(1, 4, 7, NA, 10)
get_quartile1(a,na.rm = TRUE)

#' @title get_quartile3
#' @description compute the 75th percentiles of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the 75th percentiles of a vector
get_quartile3<-function(x,na.rm=TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x))
  get_quartile3<- quantile(x, prob=c(0.75))
  return(get_quartile3)
}

a <- c(1, 4, 7, NA, 10)
get_quartile3(a,na.rm = TRUE)


#' @title count_missing
#' @description count the length of the missing values form the vector
#' @param x numeric vector, data frame
#' @param na.rm whether to remove missing values
#' @return vector in standardized units

count_missing<- function(x) {
  x1<- length(x)- length(remove_missing(x))
  return(x1)
}

a <- c(1, 4, 7, NA, 10)
count_missing(a)


#' @title get_percentile10
#' @description compute the 10th percentiles of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the 10th percentiles of a vector
#' @sample
get_percentile10<- function(x,na.rm=TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x))
  get_percentile10<- quantile(x, prob=c(0.10))
  return(get_percentile10)
}

a <- c(1, 4, 7, NA, 10)
get_percentile10(a,na.rm = TRUE)
b<- c(2,3,4,NA)
get_percentile10(b,na.rm = TRUE)

#' @title get_percentile90
#' @description compute the 90th percentiles of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the 90th percentiles of a vector
#' @sample
get_percentile90<-function(x,na.rm=TRUE){
  stopifnot(is.numeric(x))
  warning("non-numeric argument")
  x<- sort(remove_missing(x))
  get_percentile90<- quantile(x,prob=c(0.90))
  return(get_percentile90)
}

a <- c(1, 4, 7, NA, 10)
get_percentile90(a,na.rm = TRUE)

#' @title summary_stats
#' @description compute the summary of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the summary of a vector
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

a <- c(1, 4, 7, NA, 10)
stats<- summary_stats(a)
stats


#' @title print_stats
#' @description compute the summary of a vector in a nice format
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the summary of a vector in a nice format
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
print_stats(stats)


#' @title rescale100
#' @description compute a rescaled vector with a potential scale from 0 to 100 by 3 arguments: 
#' a numeric vector x, a minimum xmin, and a maximum xmax
#' the summary of a vector in a nice format 
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the rescaled vector
rescale100<- function(x, xmin, xmax){
  z<- 100*(x - xmin)/(xmax-xmin)
  return(z)
}  
  
b <- c(18, 15, 16, 4, 17, 9)
rescale100(b, xmin = 0, xmax = 20)

#' @title drop_lowest
#' @description drop the lowest of a vector
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned
#' @return the new vector after dropping the lowest value
drop_lowest1<- function(x,na.rm = TRUE) {
  x<- sort(x, decreasing = TRUE)
  n<- length(x)
  z<-x[1: n-1]
  return(z)
}

b <- c(10, 10, 8.5, 4, 7, 9)
drop_lowest1(b)


#way2
drop_lowest<- function(x,na.rm =  TRUE){
  x<-drop(which.min(x))
  return(x)
}

b <- c(10, 10, 8.5, 4, 7, 9)
drop_lowest(b)


#' @title score_homework
#' @description takes a numeric vector of homework scores (length n), compute a single homework value(with or without the lowest score), If drop = TRUE, the lowest HW score must be dropped. The function = the average of the homework scores
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned, drop whether dropping the lowest value
#' @return the average of the homework scores with or without dropping the lowest value
score_homework<- function(x, shall_i_drop){
  if(shall_i_drop == TRUE){
    x <- drop_lowest(x) }
    n<- 0 
    for(i in 1:length(x)){
      n<- n + x[i]
    }
    avg<- n/length(x)
    return(avg)
}
    
hws <- c(100, 80, 30, 70, 75, 85)
score_homework(hws, shall_i_drop = TRUE)
score_homework(hws, shall_i_drop = FALSE)
#' @title score_quiz
#' @description takes a numeric vector of quiz scores (of length n), and an optional logical argument drop to compute a single quiz value
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned. If drop = TRUE, the lowest quiz score must be dropped
#' @return the average of the quiz scores
score_quiz<- function(x, shall_i_drop){
    if(shall_i_drop == TRUE){
      x <- drop_lowest(x) }
    n<- 0 
    for(i in 1:length(x)){
      n<- n + x[i]
    }
    avg<- n/length(x)
    return(avg)
  }  

quizzes <- c(100, 80, 70, 0)
score_quiz(quizzes, shall_i_drop = TRUE)
score_quiz(quizzes, shall_i_drop = FALSE)

#' @title score_lab
#' @description takes a numeric vector of  lab attendance
#' @param x numeric vector
#' @param na.rm whether NA should be removed, if not, NA will be returned. 
#' @return the lab score
score_lab<- function(x){
  if(x== 11 | x == 12) {
    print("100")
   } else if(x== 10){
     print("80")
  } else if(x==9) {
    print("60")
  } else if(x==8) {
    print("40")
  } else if(x==7) {
      print("20")
  } else
      print("0")
}

score_lab(12)
score_lab(10)
score_lab(6)


#7) Comments and Reflections
#Reflect on what was hard/easy, problems you solved, helpful tutorials you read, etc.
#• Was this your first time writing unit tests?
##yes. And yes it's difficult.
#• On a scale from 0 to 10, how confusing you found the logic of testthat tests? (0 not at all, 10 very confusing)
##8
#• Was this your first time working with ggvis?
#yes.
#• On a scale from 0 to 10, how confusing you found the syntax of ggvis? (0 not at all,10 very confusing)
#9
#• Was this your first time working with conditional panels in shiny?
#kinda. Also did something similar on the lab assignment.

#• On a scale from 0 to 10, how challenging you found to work with the conditional panels?
#(0 not at all, 10 very challenging)
#7
#• So far we’ve exposed you to three graphing paradigms in R: base plots, ggplot, and
#now ggvis. Which do you like the most and why?
#ggplot, easy to understand.
#• Did anyone help you completing the assignment? If so, who?
#GSI, youtube, google
#• How much time did it take to complete this HW?
#3days
#• What was the most time consuming part?
#function, unit test, shiny app