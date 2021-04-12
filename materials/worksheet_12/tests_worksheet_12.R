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

test_1.1 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer1.1), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
    print("Success!")
}

test_1.2 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(paste(answer1.2, collapse="")), 'd04127a9755e9ea38971707b06bd7127')
    })
    print("Success!")
}

test_1.3 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer1.3), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_1.4 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer1.4), 'c1f86f7430df7ddb256980ea6a3b57a4')
    })
    print("Success!")
}

test_1.5 <- function(){
    test_that('one_sample_estimates should have one column named mean, and one row.', {
        expect_equal(int_round(nrow(one_sample_estimates), 0), 1)
        expect_equal(int_round(ncol(one_sample_estimates), 0), 1)
        expect_equal(digest(paste(sort(colnames(one_sample_estimates)), collapse = "")), '01e0708f75fc4f568f278b875b2e0740')
        expect_equal(digest(int_round(one_sample_estimates$mean[1], 2)), 'c054e6da6a916431a27931c4e3a1efe5')
    })
    print("Success!")
}

test_1.6 <- function(){
    test_that("boot1 should have 2 columns, named replicate and age", {
        expect_equal(digest(paste(sort(colnames(boot1)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    test_that("boot1 have 40 rows (the same number of observations as one_sample)", {
        expect_equal(int_round(nrow(boot1), 0), 40)
    })
    test_that("boot1 does not have the correct values in the age column", {
        expect_equal(digest(int_round(sum(boot1$age), 2)), '112ddeb87a12f6976a1d15f6612eda87')
    })
    test_that("size and reps do not contain the correct values", {
        expect_equal(digest(int_round(sum(as.integer(unlist(attr(boot1, "groups")))), 2)), '67a199c96b75217a12f8fa73c51e93fc')
    })
    print("Success!")
}

test_1.7 <- function() {
    test_that("Answer is incorrect", {
        expect_equal(digest(answer1.7), 'c1f86f7430df7ddb256980ea6a3b57a4')
    })
    print("Success!")
}

test_1.8 <- function() {
    properties <- c(boot1_dist$layers[[1]]$mapping, boot1_dist$mapping)
    labels <- boot1_dist$labels
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
    })
    test_that('boot1_dist should be a histogram.', {
        expect_true("GeomBar" %in% class(boot1_dist$layers[[1]]$geom))
    })
    test_that('boot1 data should be used to create the histogram', {
        expect_equal(int_round(nrow(boot1_dist$data), 0), 40)
        expect_equal(digest(int_round(sum(boot1_dist$data), 2)), 'd3e914baed4511182de1e98d25219ac8')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_false((labels$x) == 'age')
        expect_false(is.null(labels$title))
    })
    print("Success!")
}

test_1.9 <- function(){
    test_that("boot6 should have 2 columns, named replicate and age", {
        expect_equal(digest(paste(sort(colnames(boot6)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    test_that("boot6 have 240 rows (six times the number of observations in one_sample)", {
        expect_equal(int_round(nrow(boot6), 0), 240)
    })
    test_that("boot6 does not have the correct values in the age column", {
        expect_equal(digest(int_round(sum(boot6$age), 2)), 'f3f7f979ba3e6a29874aac628c26ef4f')
    })
    test_that("size and reps do not contain the correct values", {
        expect_equal(digest(int_round(sum(as.integer(unlist(attr(boot6, "groups")))), 2)), 'c553d74ed95c022e74dce82e82d6e6dd')
    })
    print("Success!")
}

test_2.0 <- function(){
    properties <- c(boot6_dist$layers[[1]]$mapping, boot6_dist$mapping)
    labels <- boot6_dist$labels
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
    })
    test_that('boot6_dist should be a histogram.', {
        expect_true("GeomBar" %in% class(boot6_dist$layers[[1]]$geom))
    })
    test_that('boot6 data should be used to create the histogram', {
        expect_equal(int_round(nrow(boot6_dist$data), 0), 240)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_false((labels$x) == 'age')
        expect_false(is.null(labels$title))
    })
    test_that('boot6_dist should use facet_wrap.', {
        expect_true("FacetWrap" %in% class(boot6_dist$facet))
    })
    print("Success!")
}

test_2.1 <- function(){
    test_that('boot6_means should have 2 columns (named replicate & mean), and six rows.', {
        expect_equal(int_round(nrow(boot6_means), 0), 6)
        expect_equal(int_round(ncol(boot6_means), 0), 2)
        expect_equal(digest(paste(sort(colnames(boot6_means)), collapse = "")), '35d687b4f0369a9d4e0a6ef74556908e')
        expect_equal(digest(int_round(boot6_means$mean[1], 2)), '1940ea892300bba15c54ed5bdbda7cb9')
    })
    print("Success!")
}

test_2.2 <- function(){
    test_that("boot1000 should have 2 columns, named replicate and age", {
        expect_equal(digest(paste(sort(colnames(boot1000)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    test_that("boot1000 have 40000 rows (1000 times the number of observations in one_sample)", {
        expect_equal(int_round(nrow(boot1000), 0), 40000)
    })
    test_that("boot1000 does not have the correct values in the age column", {
        expect_equal(digest(int_round(sum(boot1000$age), 2)), '81452ed8488b320217742924137c2e99')
    })
    test_that("size and reps do not contain the correct values", {
        expect_equal(digest(int_round(sum(as.integer(unlist(attr(boot1000, "groups")))), 0)), 'c611e93a1a0b0bdeb5e0c5acf678ee5b')
    })
    print("Success!")
}

test_2.3 <- function(){
    test_that('boot1000_means should have 2 columns (named replicate & mean), and 1000 rows.', {
        expect_equal(int_round(nrow(boot1000_means), 0), 1000)
        expect_equal(int_round(ncol(boot1000_means), 0), 2)
        expect_equal(digest(paste(sort(colnames(boot1000_means)), collapse = "")), '35d687b4f0369a9d4e0a6ef74556908e')
        expect_equal(digest(int_round(boot1000_means$mean[1], 2)), '1940ea892300bba15c54ed5bdbda7cb9')
    })
    print("Success!")
}

test_2.4 <- function(){
    properties <- c(boot_est_dist$layers[[1]]$mapping, boot_est_dist$mapping)
    labels <- boot_est_dist$labels
    test_that('mean should be on the x-axis.', {
        expect_true("mean" == rlang::get_expr(properties$x))
    })
    test_that('boot_est_dist should be a histogram.', {
        expect_true("GeomBar" %in% class(boot_est_dist$layers[[1]]$geom))
    })
    test_that('boot1000_means data should be used to create the histogram', {
        expect_equal(int_round(nrow(boot_est_dist$data), 0), 1000)
        expect_equal(digest(int_round(sum(boot_est_dist$data), 2)), 'f84934414055b43f674c20306aaf69d9')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_false((labels$x) == 'age')
        expect_false(is.null(labels$title))
    })
    print("Success!")
}

test_2.5 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.5), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
    print("Success!")
}

test_2.6 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.6), '05ca18b596514af73f6880309a21b5dd')
    })
    print("Success!")
}

test_2.7 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.7), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
    print("Success!")
}

test_2.8 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.8), '05ca18b596514af73f6880309a21b5dd')
    })
    print("Success!")
}

test_2.9 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.9), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
    print("Success!")
}
