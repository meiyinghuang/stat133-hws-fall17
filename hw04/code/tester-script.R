

devtools::install_github("r-lib/testthat")
# test script
library(testthat)
# source in functions to be tested
source('../code/functions.R')

sink('../output/test-reporter.txt')

test_file('../code/tests.R')
sink()
