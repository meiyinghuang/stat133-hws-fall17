# Script containing unit tests
devtools::install_github("r-lib/testthat")
# test script
library(testthat)
# source in functions to be tested
source('../code/functions.R')

library('RUnit')

# devtools::load_all()
# devtools::use_testthat()
# devtools::test("/hw04/code/tests")
# library(stringr)


context("remove_missing")
test_that("remove the missing values form a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(remove_missing(a), c(1, 4, 7,10))
  expect_equal(remove_missing(a), seq(1,10, by=3))
  expect_equivalent(remove_missing(a), c(1, 4, 7,10))
  expect_identical(remove_missing(a), c(1, 4, 7,10))
  #expect_lte(remove_missing(a), c(1, 4, 7,10))
  
  #expect_failure(remove_missing(a), c(1, 4, 7,10))
})

context("get_min")
test_that("the minimum value of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_min(a), which.min(a))
  expect_equivalent(get_min(a), which.min(a))
  expect_lte(get_min(a), which.min(a))
  expect_identical(get_min(a), c(1))
  })


context("get_percentile10")
test_that("the 10th percentiles of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_percentile10(a), 
               quantile(a, prob=c(0.10), na.rm=TRUE))
  expect_equivalent(get_percentile10(a), 
                    quantile(a, prob=c(0.10), na.rm=TRUE))
  expect_identical(get_percentile10(a), 
                   quantile(a, prob=c(0.10), na.rm=TRUE))
  expect_lte(get_percentile10(a), 
             quantile(a, prob=c(0.10), na.rm=TRUE))
  })
  

context("get_quartile1")
test_that("the 25th percentiles of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_quartile1(a), 
               quantile(a, prob=c(0.25), na.rm=TRUE))
  expect_equivalent(get_quartile1(a), 
                    quantile(a, prob=c(0.25), na.rm=TRUE))
  expect_identical(get_quartile1(a), 
                   quantile(a, prob=c(0.25), na.rm=TRUE))
  expect_lte(get_quartile1(a), 
             quantile(a, prob=c(0.25), na.rm=TRUE))
})


context("get_med")
test_that("the median value of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_med(a), c(5.5))
  expect_equivalent(get_med(a), c(5.5))
  expect_identical(get_med(a), c(5.5))
  expect_lte(get_med(a), 5.5)
})


context("get_avg")
test_that("the average value of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_avg(a), c(5.5))
  expect_equivalent(get_avg(a), c(5.5))
  expect_identical(get_avg(a), c(5.5))
  expect_lte(get_avg(a), 5.5)
})


context("get_quartile3")
test_that("the 75th percentiles of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_quartile3(a), 
               quantile(a, prob=c(0.75), na.rm=TRUE))
  expect_equivalent(get_quartile3(a), 
                    quantile(a, prob=c(0.75), na.rm=TRUE))
  expect_identical(get_quartile3(a), 
                   quantile(a, prob=c(0.75), na.rm=TRUE))
  expect_lte(get_quartile3(a), 
             quantile(a, prob=c(0.75), na.rm=TRUE))
})


context("get_percentile90")
test_that("the 90th percentiles of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_percentile90(a), 
               quantile(a, prob=c(0.90), na.rm=TRUE))
  expect_equivalent(get_percentile90(a), 
                    quantile(a, prob=c(0.90), na.rm=TRUE))
  expect_identical(get_percentile90(a), 
                   quantile(a, prob=c(0.90), na.rm=TRUE))
  expect_lte(get_percentile90(a), 
             quantile(a, prob=c(0.90), na.rm=TRUE))
})


context("get_max")
test_that("the maximum value of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_max(a), c(10))
  expect_equivalent(get_max(a), c(10))
  expect_identical(get_max(a), c(10))
  expect_lte(get_max(a), 10)
})

context("get_range")
test_that("the range of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_range(a), c(9))
  expect_equivalent(get_range(a), c(9))
  expect_identical(get_range(a), c(9))
  expect_lte(get_range(a), 9)
})


context("get_sd")
test_that("the standard deviation of a vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_equal(get_sd(a), sd(a, na.rm=TRUE))
  expect_equivalent(get_sd(a), sd(a, na.rm=TRUE))
  expect_identical(get_sd(a), sd(a, na.rm=TRUE))
  expect_lte(get_sd(a), sd(a, na.rm=TRUE))
})

context("count_missing")
test_that("the length of the missing values form the vector",{
  a <- c(1, 4, 7, NA, 10)
  expect_length(count_missing(a), 1)
  
  #expect_equal(count_missing(a), length(!is.numeric(a)))
  
  #expect_success(count_missing(a), 1)
  #expect_match(count_missing(a["NA"]), 1)
  
  #expect_gt(count_missing(a), 1)
  #expect_equal(count_missing(a), c(1))
  #expect_equivalent(count_missing(a), c(1))
  #expect_identical(count_missing(a), c(1))
  #expect_lte(count_missing(a), c(1))
})

context("summary_stats")
test_that("the the summary of a vector",{
  # expect_equal(summary_stats(a), 
              # c(1, 1.9, 3.25, 5.5, 5.5, 7.75, 9.1, 10, 9, 3.872983, 1))
  # expect_equivalent(summary_stats(a), 
             #  c(1, 1.9, 3.25, 5.5, 5.5, 7.75, 9.1, 10, 9, 3.872983, 1))
  # expect_identical(summary_stats(a), 
            #  c(1, 1.9, 3.25, 5.5, 5.5, 7.75, 9.1, 10, 9, 3.872983, 1))
  #expect_lte(summary_stats(a), 
          # c(1, 1.9, 3.25, 5.5, 5.5, 7.75, 9.1, 10, 9, 3.872983, 1))
})


context("rescale100")
test_that("rescale of a vector",{
  a <- c(18, 15, 16, 4, 17, 9)
  expect_equal(rescale100(a,xmin = 0, xmax = 20),
               c(90,75,80,20,85,45))
  expect_equivalent(rescale100(a,xmin = 0, xmax = 20), 
                    c(90,75,80,20,85,45)) 
  expect_identical(rescale100(a,xmin = 0, xmax = 20), 
                   c(90,75,80,20,85,45))
  expect_that(rescale100(a,xmin = 0, xmax = 20), equals(c(90,75,80,20,85,45)))
})

context("drop_lowest")
test_that("drop the lowest value of a vector",{
  b <- c(10, 10, 8.5, 4, 7, 9)
  expect_equal(drop_lowest(b), c(10,10,9,8.5,7))
  expect_equivalent(drop_lowest(b),c(10,10,9,8.5,7))
  expect_identical(drop_lowest(b), c(10,10,9,8.5,7))
  expect_that(drop_lowest(b),equals(c(10,10,9,8.5,7)))
})


context("score_homework")
test_that("the final hw grade", {
  b<- c(100, 80, 30, 70, 75, 85)
  expect_equal(score_homework(b,shall_i_drop = TRUE), c(82))
  expect_equivalent(score_homework(b,shall_i_drop = TRUE),c(82))
  expect_identical(score_homework(b,shall_i_drop = TRUE), c(82))
  expect_that(score_homework(b,shall_i_drop = TRUE),equals(c(82)))

  expect_equal(score_homework(b,shall_i_drop = FALSE), mean(b))
  expect_equivalent(score_homework(b,shall_i_drop = FALSE),mean(b))
  expect_identical(score_homework(b,shall_i_drop = FALSE), mean(b))
  expect_that(score_homework(b,shall_i_drop = FALSE),equals(mean(b)))})


context("score_quiz")
test_that("the average of the quiz scores",{
  b <- c(100, 80, 70, 0)
  d<- c(100,80,70)
  expect_equal(score_quiz(b,shall_i_drop = TRUE), mean(d))
  expect_equivalent(score_quiz(b,shall_i_drop = TRUE),mean(d))
  expect_identical(score_quiz(b,shall_i_drop = TRUE), mean(d))
  expect_that(score_quiz(b,shall_i_drop = TRUE),equals(mean(d)))
  
  expect_equal(score_quiz(b,shall_i_drop = FALSE), mean(b))
  expect_equivalent(score_quiz(b,shall_i_drop = FALSE),mean(b))
  expect_identical(score_quiz(b,shall_i_drop = FALSE), mean(b))
  expect_that(score_quiz(b,shall_i_drop = FALSE), equals(mean(b)))})


context("score_lab")
#test_that("the lab score",{
  #expect_that(score_lab(12), is_ture(100))})
  #expect_equal(score_lab(c(12,10,6), na.rm= na.rm), c(100,80,0))
  #expect_equivalent(score_lab(b),c(100,80,0))
  #expect_identical(score_lab(b),c(100,80,0))
  #expect_that(score_lab(12), (100,80,60))})
  





test_that("standardize works", {
  a <- c(1, 2, 3)
  expect_equal(standardize(a), c(-1, 0, 1))
  expect_equal(standardize(b), rep(NA_real_, 4))
  expect_equal(standardize(b, na.rm = TRUE), c(-1, 0, 1, NA))
})

context("standardize function")
test_that("standardize works", {
  a <- c(1, 2, 3)
  expect_equal(standardize(a), c(-1, 0, 1))
  expect_equal(standardize(b), rep(NA_real_, 4))
  expect_equal(standardize(b, na.rm = TRUE), c(-1, 0, 1, NA))
})

test_that("standardize throws error", {
  strings <- c('1', '2', '3')
  expect_error(standardize(strings))
})


context("rescale function")

test_that("rescale works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  expect_equal(rescale(a), c(0, 0.5, 1))
  expect_equal(rescale(b), rep(NA_real_, 4))
  expect_equal(rescale(b, na.rm = TRUE), c(0, 0.5, 1, NA))
})

test_that("rescale throws error", {
  strings <- c('1', '2', '3')
  expect_error(rescale(strings))
})


