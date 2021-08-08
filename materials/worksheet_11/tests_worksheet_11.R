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

# function to extract attributes from cowplot objects
# source: https://stackoverflow.com/questions/54051576/extracting-individual-plot-details-from-combined-plot-in-cowplot-for-unit-test?answertab=votes#tab-top
fun <- function(p, what) {
  unlist(sapply(p$layers, function(x) {
    idx <- which(x$geom_params$grob$layout$name == what)
    x$geom_params$grob$grobs[[idx]]$children[[1]]$label
  }))
}

test_1.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.0), '3a5505c06543876fe45598b5e5e5195d') 
    })
    print("Success!")
}

test_1.1 <- function(){
    test_that('variables should be assigned numeric values (do not include the definition in your answer, just the number associated with the definition)', {
    values <- c(point_estimate, population, random_sampling, representative_sampling, population_parameter, sample, observation, sampling_distribution)
        expect_is(values, 'numeric')
    })

    test_that('At least one term-definition match is incorrect', {
        expect_equal(digest(int_round(point_estimate, 0)), '25e6a154090e35101d7678d6f034353a')
        expect_equal(digest(int_round(population, 0)), '4b5630ee914e848e8d07221556b0a2fb')
        expect_equal(digest(int_round(random_sampling, 0)), 'c01f179e4b57ab8bd9de309e6d576c48')
        expect_equal(digest(int_round(representative_sampling, 0)), '7c7124efff5c7039a1b1e7cba65c5379')
        expect_equal(digest(int_round(population_parameter, 0)), '11946e7a3ed5e1776e81c0f0ecd383d0')
        expect_equal(digest(int_round(sample, 0)), 'dd4ad37ee474732a009111e3456e7ed7')
        expect_equal(digest(int_round(observation, 0)), '9d08099943f8627959cfb8ecee0d2f5d')
        expect_equal(digest(int_round(sampling_distribution, 0)), '234a2a5581872457b9fe1187d1616b13')
    })
    print("Success!")
}

test_1.2 <- function(){
    properties <- c(pop_dist$layers[[1]]$mapping, pop_dist$mapping)
    labels <- pop_dist$labels
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
        })
    test_that('pop_dist should be a histogram.', {
        expect_true("GeomBar" %in% class(pop_dist$layers[[1]]$geom))
        })
    test_that('can_seniors data should be used to create the histogram', {
        expect_equal(int_round(nrow(pop_dist$data), 0), 1027941)
        expect_equal(digest(int_round(sum(pop_dist$data$age), 0)), '0a65b77971cd131982c7117a5ab90242')
    })
    test_that('Labels on the x axis should be descriptive and human readable.', {
        expect_false((labels$x) == 'age')
        })
    print("Success!")
}

test_1.3 <- function(){
    test_that('pop_parameters has 3 columns and one row, with column names pop_mean, pop_med and pop_sd.', {
        expect_equal(int_round(nrow(pop_parameters), 0), 1)
        expect_equal(int_round(ncol(pop_parameters), 0), 3)
        expect_equal(digest(paste(sort(colnames(pop_parameters)), collapse = "")), '723d282ea6dad216da6b1074ca7cf688')
    })
    print("Success!")
}

test_1.4 <- function(){
    test_that('sample_1 should have 2 columns and 40 rows', {
        expect_equal(int_round(nrow(sample_1), 0), 40)
        expect_equal(int_round(ncol(sample_1), 0), 2)
    })
    test_that('the column names of sample_1 should be replicate and age', {
        expect_equal(digest(paste(sort(colnames(sample_1)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    print("Success!")
}

test_1.5 <- function(){
    properties <- c(sample_1_dist$layers[[1]]$mapping, sample_1_dist$mapping)
    labels <- sample_1_dist$labels
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
    })
    test_that('sample_1_dist should be a histogram.', {
        expect_true("GeomBar" %in% class(sample_1_dist$layers[[1]]$geom))
    })
    test_that('sample_1 data should be used to create the histogram', {
        expect_equal(int_round(nrow(sample_1_dist$data), 0), 40)
        expect_equal(digest(int_round(sum(sample_1_dist$data$age), 2)), 'f856ba7ffab8e669473a2ee7bf49de52')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_false((labels$x) == 'age')
        expect_false(is.null(labels$title))
    })
    print("Success!")
}

test_1.6 <- function(){
    test_that('sample_1_estimates should have at least 3 columns, and 1 row', {
        expect_equal(int_round(nrow(sample_1_estimates), 0), 1)
        expect_true(int_round(ncol(sample_1_estimates), 0) >= 3)
    })
    test_that('sample_1_estimates has columns with correct names', {
            expect_true("sample_1_mean" %in% colnames(sample_1_estimates))
            expect_true("sample_1_med" %in% colnames(sample_1_estimates))
            expect_true("sample_1_sd" %in% colnames(sample_1_estimates))
    })
    print("Success!")
}

test_1.7 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.7), '475bf9280aab63a82af60791302736f6') 
    })
    print("Success!")
}


test_1.8.0 <- function(){
        test_that('sample_2 should have 2 columns and 40 rows', {
        expect_equal(int_round(nrow(sample_2), 0), 40)
        expect_equal(int_round(ncol(sample_2), 0), 2)
    })
    test_that('the column names of sample_2 should be replicate and age', {
        expect_equal(digest(paste(sort(colnames(sample_2)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    properties <- c(sample_2_dist$layers[[1]]$mapping, sample_2_dist$mapping)
    labels <- sample_2_dist$labels
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
    })
    test_that('sample_2_dist should be a histogram.', {
        expect_true("GeomBar" %in% class(sample_2_dist$layers[[1]]$geom))
    })
    test_that('sample_2 data should be used to create the histogram', {
        expect_equal(int_round(nrow(sample_2_dist$data), 0), 40)
        expect_equal(digest(int_round(sum(sample_2_dist$data$age), 2)), '199d472897c57c820c8c694f44d7786c')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_false((labels$x) == 'age')
        expect_false(is.null(labels$title))
    })
    test_that('sample_2_estimates should have at least 3 columns, and 1 row', {
        expect_equal(int_round(nrow(sample_2_estimates), 0), 1)
        expect_true(int_round(ncol(sample_2_estimates), 0) >= 3)
    })
    test_that('sample_2_estimates has columns with correct names', {
            expect_true("sample_2_mean" %in% colnames(sample_2_estimates))
            expect_true("sample_2_med" %in% colnames(sample_2_estimates))
            expect_true("sample_2_sd" %in% colnames(sample_2_estimates))
    })
    print("Success!")
}

test_1.8.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.8.1), '475bf9280aab63a82af60791302736f6') 
    })
    print("Success!")
}

test_1.9 <- function(){
    test_that('samples should have 60000 rows and 2 columns', {
        expect_equal(int_round(ncol(samples), 0), 2)
        expect_equal(int_round(nrow(samples), 0), 60000)
    })
    test_that('the column names of samples should be replicate and age', {
        expect_equal(digest(paste(sort(colnames(samples)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    print("Success!")
}

test_2.0 <- function(){
    test_that('sample_estimates should have 1500 rows and 2 columns', {
        expect_equal(int_round(ncol(sample_estimates), 0), 2)
        expect_equal(int_round(nrow(sample_estimates), 0), 1500)
    })
    test_that('the column names of sample_estimates should be replicate and sample_mean', {
        expect_equal(digest(paste(sort(colnames(sample_estimates)), collapse = "")), '7453089f8086e9a98a067f3eeac63363')
    })
    print("Success!")
}

test_2.1 <- function(){
    properties <- c(sampling_distribution$layers[[1]]$mapping, sampling_distribution$mapping)
    labels <- sampling_distribution$labels
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution should be a histogram.', {
        expect_true("GeomBar" %in% class(sampling_distribution$layers[[1]]$geom))
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(int_round(nrow(sampling_distribution$data), 0), 1500)
        expect_equal(digest(int_round(sum(sampling_distribution$data$sample_mean), 2)), 'e20a3a6689ccb7122ce8aaa71bab55bf')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_false((labels$x) == 'age')
        expect_false(is.null(labels$title))
    })
    print("Success!")
}

test_2.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(int_round(answer2.2, 2)), '0ddc7e7a0d2654650cba2f2a15cbca52') 
    })
    print("Success!")
}

test_2.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.3), '3a5505c06543876fe45598b5e5e5195d') 
    })
    print("Success!")
}

test_2.4 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(tolower(answer2.4)), '05ca18b596514af73f6880309a21b5dd') 
    })
    print("Success!")
}

test_2.5 <- function(){
    properties <- c(sampling_distribution_20$layers[[1]]$mapping, sampling_distribution_20$mapping)
    labels <- sampling_distribution_20$labels
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution should be a histogram.', {
        expect_true("GeomBar" %in% class(sampling_distribution_20$layers[[1]]$geom))
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(int_round(nrow(sampling_distribution_20$data), 0), 1500)
        expect_equal(digest(int_round(sum(sampling_distribution_20$data$sample_mean), 2)), '49a66adc63b05e7e8f90b66202de0b84')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have the title n = 20.', {
        expect_false((labels$x) == 'age')
        expect_equal(labels$title, "n = 20")
    })

    print("Success!")
}

test_2.6 <- function(){
    properties <- c(sampling_distribution_100$layers[[1]]$mapping, sampling_distribution_100$mapping)
    labels <- sampling_distribution_100$labels
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution should be a histogram.', {
        expect_true("GeomBar" %in% class(sampling_distribution_100$layers[[1]]$geom))
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(int_round(nrow(sampling_distribution_100$data), 0), 1500)
        expect_equal(digest(int_round(sum(sampling_distribution_100$data$sample_mean), 2)), '59c92b151db8f38ba93a364fd62ae7c9')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have the title n = 100.', {
        expect_false((labels$x) == 'age')
        expect_equal(labels$title, "n = 100")
    })

    print("Success!")
}

test_2.7 <- function(){
    test_that('object is named sampling_distribution_panel.', {
        expect_true(exists("sampling_distribution_panel"))
    })
    test_that('sampling distributions are plotted side-by-side with the correct titles of n = 20, "n = 40, and n = 100', {
        expect_equal(fun(sampling_distribution_panel, "title"), c("n = 20", "n = 40", "n = 100"))
    })
    print("Success!")
}

test_2.8 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.8), 'c1f86f7430df7ddb256980ea6a3b57a4') 
    })
    print("Success!")
}

test_2.9 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(tolower(answer2.9)), 'd2a90307aac5ae8d0ef58e2fe730d38b') 
    })
    print("Success!")
}
