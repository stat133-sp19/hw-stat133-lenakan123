##test.R 

# load the source code of the functions to be tested
setwd("C:/Users/lenak/Desktop/hw-stat133/lab09")
source("functions.R")
# context with one test that groups expectations
context("Test for range value") 

##Your Turn: test stat_range() with the rest of the testing vectors: y,z,w

test_that("range works as expected", {
  y <- c(1,2,3,4,NA)
  
  expect_equal(stat_range(y), 4)
  expect_length(stat_range(y), 1)
  expect_type(stat_range(y), 'double')
})

test_that("range works as expected", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(stat_range(z), 4)
  expect_length(stat_range(z), 1)
  expect_type(stat_range(z), 'double')
})

test_that("range works as expected", {
  w <- letters[1:5]
  
  expect_equal(stat_range(w), 4)
  expect_length(stat_range(w), 1)
  expect_type(stat_range(w), 'double')
})

test_that("range works as expected", {
  y <- c(1,2,3,4,NA)
  
  expect_equal(stat_range(y), NA_real_)
  expect_length(stat_range(y), 1)
})

test_that("range works as expected", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(stat_range(z), 1L)
  expect_length(stat_range(z), 1)
  expect_type(stat_range(z), 'integer')
})


test_that("range works as expected", {
  w <- letters[1:5]
  
  expect_type(stat_range(w), 'double')
})


##UNIT TESTS FOR FUNCTIONS OF CENTER AND SPREAD

test_that("centers works as expected", {
  x <- c(1:5)
  expect_equal(stat_centers(x), c(3,3))
})

test_that("spreads work as expected",{
  x<- c(1:5)
  expect_equal(stat_spreads(x), c(2,2,2))
})


##PART 2) Basics of String Manipulation*



##assuming that your working directory is "lab09/, 
#to run the tsts from R console, use the function test_file() by passing the path of the file tests.R"
test_file("tests.R")
