# +
library(testthat)
library(digest)

#' Round double to precise integer
#'
#' `int_round` works to create an integer corresponding to a number that is 
#' tested up to a particular decimal point of precision. This is useful when 
#' there is a need to compare a numeric value using hashes.
#'
#' @param x Double vector of length one.
#' @param digits Double vector of length one to specify decimal point of precision. Negative numbers can be used to specifying significant digits > 0.1.
#'
#' @return Integer vector of length one corresponding to a particular decimal point of precision.
#'
#' @examples
#' # to get an integer up to two decimals of precision from 234.56789
#' int_round(234.56789, 2)
#' 
#' to get an integer rounded to the hundred digit from 234.56789
#' int_round(234.56789, -2)
int_round <- function(x, digits){
    x = x*10^digits
    xint = as.integer(x)
    xint1 = xint + 1L
    if (abs(xint - x) < abs(xint1 - x)){
        return(xint)
    }
    else {
        return(xint1)
    }
}

test_that('int_round gives expected types', {
    expect_equal(typeof(int_round(234.56789, 2)), "integer")
    expect_equal(typeof(int_round(234.56789, -2)), "integer")
    expect_equal(typeof(int_round(234L, 2)), "integer")
})

test_that('int_round gives expected values', {
    expect_equal(int_round(234.56789, 2), 23457)
    expect_equal(int_round(234.56789, -2), 2)
    expect_equal(int_round(234L, 2), 23400)
})
# -

test_3.2 <- function(){
    test_that('seconds_in_a_minute should exist and have the value of how many seconds are in a minute.', {
        expect_equal(digest(int_round(seconds_in_a_minute, 2)), 'fd54bb9b93cc9d4df5d0e1b0ef6d2588') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('seconds_in_a_hour should exist and have the value of how many seconds are in a hour.', {
        expect_equal(digest(int_round(seconds_in_an_hour, 2)), '20dbbc2a608b1e18079e3a446a9cfb38') # we hid the answer to the test here so you can't see it, but we can still run the test
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
        expect_equal(digest(int_round(sum(marathon_small$age), 2)), 'b0c43657ea54a87600e9a39a810e7d79')
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
        expect_equal(digest(int_round(nrow(marathon_filtered), 0)), 'd9509be2b148230926a2df0f355c16b2') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('marathon_filtered has the incorrect number of column', {
        expect_equal(digest(int_round(ncol(marathon_filtered), 0)), 'dd4ad37ee474732a009111e3456e7ed7') # we hid the answer to the test here so you can't see it, but we can still run the test   
        })
    test_that('marathon_filtered bmi column contains the incorrect values', {
        expect_equal(colnames(marathon_filtered), c("age", "bmi", "km5_time_seconds", "km10_time_seconds", "sex"))
        expect_equal(digest(int_round(sum(marathon_filtered$bmi), 2)), '875f8815cddc21b22a1b1eb1d5ed6ab6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.4.1 <- function(){
    test_that('marathon_male has the incorrect number of rows', {
        expect_equal(digest(int_round(nrow(marathon_male), 0)), 'd9509be2b148230926a2df0f355c16b2') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('marathon_male has the incorrect number of columns', {
        expect_equal(digest(int_round(ncol(marathon_male), 0)), 'c01f179e4b57ab8bd9de309e6d576c48') # we hid the answer to the test here so you can't see it, but we can still run the test  
        })
    test_that('marathon_male bmi and/or km10_time_seconds column(s) contains the incorrect values', {
        expect_equal(digest(int_round(sum(marathon_male$bmi), 2)), '875f8815cddc21b22a1b1eb1d5ed6ab6') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(marathon_male$km10_time_seconds, na.rm = TRUE), 2)), 'c35b7f74b3421852308c5f7722b30667') # we hid the answer to the test here so you can't see it, but we can still run the test
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
        expect_equal(digest(int_round(nrow(marathon_minutes), 0)), 'd9509be2b148230926a2df0f355c16b2') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
    test_that('marathon_minutes has the incorrect number of columns', {
        expect_equal(digest(int_round(ncol(marathon_minutes), 0)), '11946e7a3ed5e1776e81c0f0ecd383d0') # we hid the answer to the test here so you can't see it, but we can still run the test  
        })
    test_that('km10_time_minutes column does not exist contains incorrect values', {
        expect_equal(digest(int_round(sum(marathon_minutes$km10_time_minutes, na.rm = TRUE), 2)), 'df88470bb77695f5d6bccdc54dd5c6bb') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_7.6.1 <- function(){
    test_that('Solution is incorrect', {
        expect_match(digest(answer7.6.1), '3a5505c06543876fe45598b5e5e5195d')
        })
print("Success!")
    }
