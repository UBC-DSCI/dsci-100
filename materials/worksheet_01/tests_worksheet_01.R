library(testthat)
library(digest)

test_3.2 <- function(){
    test_that('seconds_in_a_minute should exist and have the value of how many seconds are in a minute.', {
        expect_equal(digest(seconds_in_a_minute), '4bdb128c943f718f5b8f347bb4b7641b') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('seconds_in_a_hour should exist and have the value of how many seconds are in a hour.', {
        expect_equal(digest(seconds_in_an_hour), 'a69521e1dbffd4cd8f6ed869a4eba073') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_4.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(title), 'c76933115bc8095b2140c11556800725') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_4.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(smallest), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_5.1 <- function(){
    test_that('Solution is incorrect, the rvest package needs to be loaded', {
        expect_that("package:rvest" %in% search() , is_true())
        })
print("Success!")
    }

test_6.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer6.1), '75f1160e72554f4270c809f041c7a776') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.0.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer7.0.1), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.0.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer7.0.2), '05ca18b596514af73f6880309a21b5dd') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.0.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer7.0.3), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.1.1 <- function(){
    test_that('Did not create an object named marathon_small', {
        expect_true(exists("marathon_small"))
        })
    test_that('marathon_small should be a data frame', {
        expect_true('data.frame' %in% class(marathon_small))
        })
    test_that('marathon_small does not contain the correct data', {
        expect_equal(dim(marathon_small), c(1833, 5))
        expect_equal(digest(round(sum(marathon_small$age))), "01d0e3ad4837c2290949b408787f5f3e")
        expect_equal(colnames(marathon_small), c("age", "bmi", "km5_time_seconds", "km10_time_seconds", "sex"))
        })
print("Success!")
    }

test_7.1.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer7.1.2), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.2.1 <- function(){
    test_that('Solution is incorrect.', {
        expect_equal(digest(number_rows), '58fac55045cec17cd9f4006f4b5ab349') # we hid the answer to the test here so you can't see it, but we can still run the test   
        })
print("Success!")
    }

test_7.3.1 <- function(){
    test_that('marathon_filtered has the incorrect number of rows', {
        expect_equal(digest(nrow(marathon_filtered)), 'd9509be2b148230926a2df0f355c16b2') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('marathon_filtered has the incorrect number of column', {
        expect_equal(digest(ncol(marathon_filtered)), 'dd4ad37ee474732a009111e3456e7ed7') # we hid the answer to the test here so you can't see it, but we can still run the test   
        })
    test_that('marathon_filtered bmi column contains the incorrect values', {
        expect_equal(colnames(marathon_filtered), c("age", "bmi", "km5_time_seconds", "km10_time_seconds", "sex"))
        expect_equal(digest(round(as.numeric(sum(marathon_filtered$bmi)))), '8d5e20de34549cf5d90abc665abdd883') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.4.1 <- function(){
    test_that('marathon_male has the incorrect number of rows', {
        expect_equal(digest(nrow(marathon_male)), 'd9509be2b148230926a2df0f355c16b2') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('marathon_male has the incorrect number of columns', {
        expect_equal(digest(ncol(marathon_male)), 'c01f179e4b57ab8bd9de309e6d576c48') # we hid the answer to the test here so you can't see it, but we can still run the test  
        })
    test_that('marathon_male bmi and/or km10_time_seconds column(s) contains the incorrect values', {
        expect_equal(digest(round(sum(as.numeric(marathon_male$bmi)))), '8d5e20de34549cf5d90abc665abdd883') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(round(sum(as.numeric(marathon_male$km10_time_seconds), na.rm = TRUE))), '94a8a4aca7bf6c04261303c3aff557da') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.4.2 <- function(){
    test_that('Solution is incorrect', {
        expect_match(digest(answer7.4.2), "a9cf135185e7fe4ae642c8dcb228cd2d")    
        })
print("Success!")
    }

test_7.4.3 <- function(){
    test_that('Solution is incorrect', {
        expect_match(digest(answer7.4.3), "edf7faf67d063030eba4ec85c6f7cc55")    
        })
print("Success!")
    }

test_7.5.1 <- function(){
    test_that('marathon_minutes has the incorrect number of rows', {
        expect_equal(digest(nrow(marathon_minutes)), 'd9509be2b148230926a2df0f355c16b2') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('marathon_minutes has the incorrect number of columns', {
        expect_equal(digest(ncol(marathon_minutes)), '11946e7a3ed5e1776e81c0f0ecd383d0') # we hid the answer to the test here so you can't see it, but we can still run the test  
        })
    test_that('km10_time_minutes column does not exist contains incorrect values', {
        expect_equal(digest(round(sum(as.numeric(marathon_minutes$km10_time_minutes), na.rm = TRUE))), 'baab288fe3905d65ebe40bd578a14723') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.6.1 <- function(){
    test_that('Solution is incorrect', {
        expect_match(digest(answer7.6.1), '3a5505c06543876fe45598b5e5e5195d')
        })
print("Success!")
    }