library(testthat)
library(digest)

test_1.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.0), '3a5505c06543876fe45598b5e5e5195d') 
    })
    print("Success!")
}

test_1.1 <- function(){
    test_that('variables should be assigned numeric values (do not include the definition in your answer, just the number associated with the definition)', {
    values <- c(point_estimate, population, random_sampling, representative_sampling, population_quantity, sample, observation, sampling_distribution)
        expect_is(values, 'numeric')
    })

    test_that('At least one term-definition match is incorrect', {
        expect_equal(digest(point_estimate), '0aee9b78301d7ec8998971363be87c03')
        expect_equal(digest(population), '6717f2823d3202449301145073ab8719')
        expect_equal(digest(random_sampling), 'db8e490a925a60e62212cefc7674ca02')
        expect_equal(digest(representative_sampling), '90a7653d717dc1553ee564aa27b749b9')
        expect_equal(digest(population_quantity), 'e5b57f323c7b3719bbaaf9f96b260d39')
        expect_equal(digest(sample), '5e338704a8e069ebd8b38ca71991cf94')
        expect_equal(digest(observation), '4a5d7d50676e6d0ea065f445d8a5539d')
        expect_equal(digest(sampling_distribution), 'dbc09cba9fe2583fb01d63c70e1555a8')
    })
    print("Success!")
}

test_1.2 <- function(){
    properties <- c(pop_dist$layers[[1]]$mapping, pop_dist$mapping)
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
        })
    test_that('pop_dist should be a histogram.', {
        expect_that("GeomBar" %in% class(pop_dist$layers[[1]]$geom), is_true())
        })
    test_that('can_seniors data should be used to create the histogram', {
        expect_equal(nrow(pop_dist$data), 1027941)
        expect_equal(round(as.numeric(sum(pop_dist$data))), 81516771)
    })
    test_that('Labels on the x axis should be descriptive and human readable.', {
        expect_that((pop_dist$labels$x) == 'age', is_false())
        })
    print("Success!")
}

test_1.3 <- function(){
    test_that('pop_quantities has 3 columns and one row, with column names pop_mean, pop_med and pop_sd.', {
        expect_equal(nrow(pop_quantities), 1)
        expect_equal(ncol(pop_quantities), 3)
        expect_equal(digest(paste(sort(colnames(pop_quantities)), collapse = "")), '723d282ea6dad216da6b1074ca7cf688')
    })
    print("Success!")
}

test_1.4 <- function(){
    test_that('sample_1 should have 2 columns and 40 rows', {
        expect_equal(nrow(sample_1), 40)
        expect_equal(ncol(sample_1), 2)
    })
    test_that('the column names of sample_1 should be replicate and age', {
        expect_equal(digest(paste(sort(colnames(sample_1)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    print("Success!")
}

test_1.5 <- function(){
    properties <- c(sample_1_dist$layers[[1]]$mapping, sample_1_dist$mapping)
    test_that('age should be on the x-axis.', {
        expect_true("age" == rlang::get_expr(properties$x))
    })
    test_that('sample_1_dist should be a histogram.', {
        expect_that("GeomBar" %in% class(sample_1_dist$layers[[1]]$geom), is_true())
    })
    test_that('sample_1 data should be used to create the histogram', {
        expect_equal(nrow(sample_1_dist$data), 40)
        expect_equal(round(as.numeric(sum(sample_1_dist$data))), 3265)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_that((sample_1_dist$labels$x) == 'age', is_false())
        expect_false(is.null(sample_1_dist$labels$title))
    })
    print("Success!")
}

test_1.6 <- function(){
    test_that('sample_1_estimates should have at least 3 columns, and 1 row', {
        expect_equal(nrow(sample_1_estimates), 1)
        expect_true(ncol(sample_1_estimates) >= 3)
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

test_1.8.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.8.1), '475bf9280aab63a82af60791302736f6') 
    })
    print("Success!")
}

test_1.9 <- function(){
    test_that('samples should have 60000 rows and 2 columns', {
        expect_equal(ncol(samples), 2)
        expect_equal(nrow(samples), 60000)
    })
    test_that('the column names of samples should be replicate and age', {
        expect_equal(digest(paste(sort(colnames(samples)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    print("Success!")
}

test_2.0 <- function(){
    test_that('sample_estimates should have 1500 rows and 2 columns', {
        expect_equal(ncol(sample_estimates), 2)
        expect_equal(nrow(sample_estimates), 1500)
    })
    test_that('the column names of sample_estimates should be replicate and sample_mean', {
        expect_equal(digest(paste(sort(colnames(sample_estimates)), collapse = "")), '7453089f8086e9a98a067f3eeac63363')
    })
    print("Success!")
}

test_2.1 <- function(){
    properties <- c(sampling_distribution$layers[[1]]$mapping, sampling_distribution$mapping)
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution should be a histogram.', {
        expect_that("GeomBar" %in% class(sampling_distribution$layers[[1]]$geom), is_true())
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(nrow(sampling_distribution$data), 1500)
        expect_equal(round(as.numeric(sum(sampling_distribution$data))), 1244931)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_that((sampling_distribution$labels$x) == 'age', is_false())
        expect_false(is.null(sampling_distribution$labels$title))
    })
    print("Success!")
}

test_2.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(round(answer2.2)), '49df39a554b7960907ee669ebb6c2071') 
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
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution should be a histogram.', {
        expect_that("GeomBar" %in% class(sampling_distribution_20$layers[[1]]$geom), is_true())
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(nrow(sampling_distribution_20$data), 1500)
        expect_equal(round(as.numeric(sum(sampling_distribution_20$data))), 1244837)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_that((sampling_distribution_20$labels$x) == 'age', is_false())
        expect_equal(sampling_distribution_20$labels$title, "n = 20")
    })

    print("Success!")
}

test_2.6 <- function(){
    properties <- c(sampling_distribution_100$layers[[1]]$mapping, sampling_distribution_100$mapping)
    test_that('sample_mean should be on the x-axis.', {
        expect_true("sample_mean" == rlang::get_expr(properties$x))
    })
    test_that('sampling_distribution should be a histogram.', {
        expect_that("GeomBar" %in% class(sampling_distribution_100$layers[[1]]$geom), is_true())
    })
    test_that('sampling_distribution data should be used to create the histogram', {
        expect_equal(nrow(sampling_distribution_100$data), 1500)
        expect_equal(round(as.numeric(sum(sampling_distribution_100$data))), 1244833)
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_that((sampling_distribution_100$labels$x) == 'age', is_false())
        expect_equal(sampling_distribution_100$labels$title, "n = 100")
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