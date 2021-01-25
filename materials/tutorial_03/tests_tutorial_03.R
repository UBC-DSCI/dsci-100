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

test_0.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(A), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(B), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(C), '6717f2823d3202449301145073ab8719') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(D), 'dbc09cba9fe2583fb01d63c70e1555a8') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(E), '0aee9b78301d7ec8998971363be87c03') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(F), '5e338704a8e069ebd8b38ca71991cf94') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.1 <- function(){
    test_that('Did not create an object named avocado', {
        expect_true(exists("avocado")) 
        })
    test_that('avocado should be a data frame.', {
        expect_true('data.frame' %in% class(avocado))
        })
    test_that('avocado does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(avocado), c(17911, 9))
        })
    test_that('avocado does not contain the correct data.', {
        expect_equal(digest(int_round(sum(avocado$average_price), 2)), '925e36908ff4830300330ada3666458c') 
        expect_equal(colnames(avocado), c("Date", "average_price", "small_hass_volume", "large_hass_volume", "extra_l_hass_volume", "type", "yr", "region", "wk"))
        })
print("Success!")
    }

test_1.2 <- function(){
    test_that('Did not create an object named cheapest', {
        expect_true(exists("cheapest")) 
        })
    test_that('cheapest should be a data frame.', {
        expect_true('data.frame' %in% class(cheapest))
        })
    test_that('avocado does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(cheapest), c(1, 2))
        })
    test_that('cheapest does not contain the correct data.', {
        expect_equal(digest(cheapest$region[0]), '5152ac13bdd09110d9ee9c169a3d9237') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(as.integer(unlist(select(cheapest, -region))), 2)), '5d6e7fe43b3b73e5fd2961d5162486fa') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.3 <- function(){
    test_that('Did not create a plot named avocado_plot', {
        expect_true(exists("avocado_plot")) 
        })
    properties <- c(avocado_plot$layers[[1]]$mapping, avocado_plot$mapping)
    test_that('total_volume should be on the x-axis.', {
        expect_true("total_volume" == rlang::get_expr(properties$x))
        })
    test_that('average_price should be on the y-axis.', {
        expect_true("average_price" == rlang::get_expr(properties$y))
        })
    test_that('region should be Houston.', {
        expect_true(unique(avocado_plot$data$region) == "Houston")
        })
    test_that('avocado_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(avocado_plot$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(avocado_plot$labels$y == 'average_price')
        expect_false(avocado_plot$labels$x == 'total_volume')
        })
print("Success!")
    }

test_3.1 <- function(){
    test_that('Did not create an object named sea_surface', {
        expect_true(exists("sea_surface")) 
        })
    test_that('sea_surface should be a data frame.', {
        expect_true('data.frame' %in% class(sea_surface))
        })
    test_that('sea_surface does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(sea_surface), c(105, 13))
        })
    test_that('sea_surface does not contain the correct data.', {
        expect_equal(digest(int_round(sum(sea_surface$Dec, na.rm = TRUE), 2)), '15045e9db8607a868e0cc475a3f7b9b8') 
        expect_equal(colnames(sea_surface), c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        })
print("Success!")
    }
