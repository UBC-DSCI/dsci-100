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
    x = x * 10^digits
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

test_revision <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(A), 'dbc09cba9fe2583fb01d63c70e1555a8') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(B), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test)
        expect_equal(digest(C), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(D), '5e338704a8e069ebd8b38ca71991cf94') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(E), '6717f2823d3202449301145073ab8719') # we hid the answer to the test here so you can't see it, but we can still run the test
    })
print("Success!")
    }

test_1.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.1), '75f1160e72554f4270c809f041c7a776') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.2), 'd2a90307aac5ae8d0ef58e2fe730d38b') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.3), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.4 <- function(){
    test_that('The tidyverse package needs to be loaded.', {
        expect_true("package:tidyverse" %in% search())
        })
print("Success!")
    }

test_1.5 <- function(){
    test_that('Did not create an object named marathon_small.', {
        expect_true(exists("marathon_small")) 
        })
    test_that('read.csv() used instead of read_csv()', {
        expect_true(class(marathon_small$sex) == "character")
        })
    test_that('marathon_small should be a data frame.', {
        expect_true('data.frame' %in% class(marathon_small))
        })
    test_that('marathon_small does not contain the correct data.', {
        expect_equal(dim(marathon_small), c(1833, 5))
        expect_equal(digest(int_round(sum(marathon_small$age), 2)), 'b0c43657ea54a87600e9a39a810e7d79')
        expect_equal(colnames(marathon_small), c("age", "bmi", "km5_time_seconds", "km10_time_seconds", "sex"))
        })
print("Success!")
    }

test_1.6 <- function(){
    test_that('Did not create an object named marathon_age.', {
        expect_true(exists("marathon_age")) 
        })
    test_that('Did not create an object named marathon_select.', {
        expect_true(exists("marathon_select")) 
        })
    test_that('marathon_age does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon_age), c(922, 5))
        })
    test_that('marathon_select does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon_select), c(922, 2))
        })
    test_that('Columns in marathon_select contain incorrect values.', {
        expect_equal(digest(int_round(sum(marathon_select$bmi), 2)), '8e852b0968d72c659936a14f778a1f48') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(marathon_select$km5_time_seconds, na.rm = TRUE), 2)), '8c35a70483c1be5d9f9ddbfbd62aca95') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.7 <- function(){
    test_that('Did not create an object named marathon_mutate.', {
        expect_true(exists("marathon_mutate")) 
        })
    test_that('Did not create an object named marathon_exact.', {
        expect_true(exists("marathon_exact")) 
        })
    test_that('marathon_mutate does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon_mutate), c(922, 3))
        })
    test_that('marathon_exact does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon_exact), c(922, 2))
        })
    test_that('Columns in marathon_exact contain incorrect values.', {
        expect_equal(digest(int_round(sum(marathon_exact$bmi), 2)), '8e852b0968d72c659936a14f778a1f48') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(marathon_exact$km5_time_minutes, na.rm = TRUE), 2)), '7f7f1d2422fdaef28da76e2f8f9c55ef') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.8 <- function(){
    test_that('Did not create a plot named marathon_plot', {
        expect_true(exists("marathon_plot")) 
        })
    
    properties <- c(marathon_plot$layers[[1]]$mapping, marathon_plot$mapping)
    test_that('bmi should be on the x-axis.', {
        expect_true("bmi" == rlang::get_expr(properties$x))
        })
    test_that('km5_time_minutes should be on the y-axis.', {
        expect_true("km5_time_minutes" == rlang::get_expr(properties$y))
        })
    test_that('marathon_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(marathon_plot$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(marathon_plot$labels$y == 'km5_time_minutes')
        expect_false(marathon_plot$labels$x == 'bmi')
        })
print("Success!")
    }

test_1.10 <- function(){
    test_that('Did not create a plot named age_vs_time', {
        expect_true(exists("age_vs_time")) 
        })
    test_that('Did not create a data frame named marathon_small_mins', {
        expect_true(exists("marathon_small_mins")) 
    })
    
    properties <- c(age_vs_time$layers[[1]]$mapping, age_vs_time$mapping)
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
        })
    test_that('km5_time_minutes should be on the y-axis.', {
        expect_true("km5_time_minutes" == rlang::get_expr(properties$y))
        })
    test_that('age_vs_time should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(age_vs_time$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(age_vs_time$labels$y == 'km5_time_minutes')
        expect_false(age_vs_time$labels$x == 'age') # removed since 'age' is human-readable
        })
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.1), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.2), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.3 <- function(){
    test_that('Did not create an object named bike_data.', {
        expect_true(exists("bike_data")) 
        })
    test_that('bike_data should be a data frame.', {
        expect_true('data.frame' %in% class(bike_data))
        })
    test_that('bike_data does not contain the correct information.', {
        expect_equal(dim(bike_data), c(731, 4))
        expect_equal(digest(int_round(sum(bike_data$casual_users), 2)), '7aff4e006e15c73072e7c5aacc5924e3')
        expect_equal(colnames(bike_data), c("temperature", "casual_users", "registered_users", "season"))
        })
    test_that('read.csv() used instead of read_csv()', {
        expect_true(class(bike_data$season) == "character")
        })
print("Success!")
    }

test_2.4 <- function(){
    test_that('Did not create an object named bike_mutate.', {
        expect_true(exists("bike_mutate")) 
        })
    test_that('bike_mutate does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(bike_mutate), c(731, 5))
        })
    test_that('Columns in bike_mutate contain incorrect values.', {
        expect_equal(digest(int_round(sum(bike_mutate$total_users), 2)), 'a48685b34390b4162a28ca604d58da19') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(bike_mutate$temperature, na.rm = TRUE), 2)), '5977c1efdc30d1d4c73b8c291c018836') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.5 <- function(){
    test_that('Did not create an object named bike_filter.', {
        expect_true(exists("bike_filter")) 
        })
    test_that('The season column in bike_filter should only contain Spring.', {
        expect_equal(unique(bike_filter$season), "Spring")
        })
    test_that('bike_filter does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(bike_filter), c(181, 5))
        })
    test_that('Columns in bike_filter contain incorrect values.', {
        expect_equal(digest(int_round(sum(bike_filter$total_users), 2)), 'ab86544bfa851e18c0b0ab0ac4f571a7') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(bike_filter$temperature, na.rm = TRUE), 2)), 'd2192d973d9361d9018ad1df3155ee24') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_3.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.1), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }
