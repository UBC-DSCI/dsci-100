library(testthat)
library(digest)

test_1.0 <- function(){
    properties <- c(pop_dist$layers[[1]]$mapping, pop_dist$mapping)
    test_that('grade should be on the x-axis.', {
        expect_true("grade" == rlang::get_expr(properties$x))
        })
    test_that('pop_dist should be a histogram.', {
        expect_that("GeomBar" %in% class(pop_dist$layers[[1]]$geom), is_true())
        })
    test_that('students_pop data should be used to create the histogram', {
        expect_equal(nrow(pop_dist$data), 10000)
        expect_equal(round(as.numeric(sum(pop_dist$data))), 700329)
    })
    test_that('Labels on the x axis should be descriptive and human readable.', {
        expect_that((pop_dist$labels$x) == 'grade', is_false())
        })
    print("Success!")
}

test_1.2 <- function(){
    test_that('pop_parameters has 3 columns and one row, with column names pop_mean, pop_med and pop_sd.', {
        expect_equal(nrow(pop_parameters), 1)
        expect_equal(ncol(pop_parameters), 3)
        expect_equal(digest(round(pop_parameters$pop_mean,1)), '2a560fd9a59ef8af4b1f1b40af6ab40d')
        expect_equal(digest(round(pop_parameters$pop_sd,1)), '03fd1d869a1a7fb2c286fa4568b080fe')
    })
    print("Success!")
}

test_1.3 <- function(){
    test_that('samples should have 7500 rows and 2 columns', {
        expect_equal(ncol(samples), 2)
        expect_equal(nrow(samples), 7500)
    })
    test_that('the column names of samples should be replicate and grade', {
        expect_equal(digest(paste(sort(colnames(samples)), collapse = "")), '0454d7f37ea4f0b0109a37b637be0481')
    })
    print("Success!")
}

test_1.4 <- function(){
    test_that('sample_estimates should have 1500 rows and 2 columns', {
        expect_equal(ncol(sample_estimates), 2)
        expect_equal(nrow(sample_estimates), 1500)
    })
    test_that('the column names of sample_estimates should be replicate and sample_mean', {
        expect_equal(digest(paste(sort(colnames(sample_estimates)), collapse = "")), '7453089f8086e9a98a067f3eeac63363')
    })
    print("Success!")
}

test_1.5 <- function(){
    properties <- c(sampling_distribution_5$layers[[1]]$mapping, sampling_distribution_5$mapping)
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution_5 should be a histogram.', {
        expect_that("GeomBar" %in% class(sampling_distribution_5$layers[[1]]$geom), is_true())
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(nrow(sampling_distribution_5$data), 1500)
        expect_equal(round(as.numeric(sum(sampling_distribution_5$data))), 1230616)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_that((sampling_distribution_5$labels$x) == 'age', is_false())
        expect_false(is.null(sampling_distribution_5$labels$title))
    })
    print("Success!")
}

test_1.8 <- function(){
    properties <- c(sampling_distribution_5$layers[[1]]$mapping, sampling_distribution_5$mapping)
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution_5 should be a histogram.', {
        expect_that("GeomBar" %in% class(sampling_distribution_5$layers[[1]]$geom), is_true())
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(nrow(sampling_distribution_5$data), 1500)
        expect_equal(round(as.numeric(sum(sampling_distribution_5$data))), 1129548)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_that((sampling_distribution_5$labels$x) == 'cups', is_false())
        expect_false(is.null(sampling_distribution_5$labels$title))
    })
    print("Success!")
}

test_2.0 <- function(){
    properties <- c(sampling_distribution_30$layers[[1]]$mapping, sampling_distribution_30$mapping)
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution_30 should be a histogram.', {
        expect_that("GeomBar" %in% class(sampling_distribution_30$layers[[1]]$geom), is_true())
    })
    test_that('sampling_distribution_30 data should be used to create the histogram', {
        expect_equal(nrow(sampling_distribution_30$data), 1500)
        expect_equal(round(as.numeric(sum(sampling_distribution_30$data))), 1129466)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_that((sampling_distribution_30$labels$x) == 'age', is_false())
        expect_false(is.null(sampling_distribution_30$labels$title))
    })
    print("Success!")
}