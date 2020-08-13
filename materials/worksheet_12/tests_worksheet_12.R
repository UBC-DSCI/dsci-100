library(testthat)
library(digest)

test_1.1 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer1.1), '81949aed6f8e18b150efa97ff46a6fc3')
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
        expect_equal(nrow(one_sample_estimates), 1)
        expect_equal(ncol(one_sample_estimates), 1)
        expect_equal(digest(paste(sort(colnames(one_sample_estimates)), collapse = "")), '01e0708f75fc4f568f278b875b2e0740')
        expect_equal(digest(round(as.numeric(one_sample_estimates$mean[1]))), '8eaea8fff4c78a39455fb520153717a1')
    })
    print("Success!")
}

test_1.6 <- function(){
    test_that("boot1 should have 2 columns, named replicate and age", {
        expect_equal(digest(paste(sort(colnames(boot1)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    test_that("boot1 have 40 rows (the same number of observations as one_sample)", {
        expect_equal(nrow(boot1), 40)
    })
    test_that("boot1 does not have the correct values in the age column", {
        expect_equal(digest(round(as.numeric(sum(boot1$age)))), 'd7e0722f1138ad8269edc6c1732ecfeb')
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
        expect_equal(nrow(boot1_dist$data), 40)
        expect_equal(digest(round(as.numeric(sum(boot1_dist$data)))), '6f48fa3c9cce9f1c310dabba246abad3')
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
        expect_equal(nrow(boot6), 240)
    })
    test_that("boot6 does not have the correct values in the age column", {
        expect_equal(digest(round(as.numeric(sum(boot6$age)))), '4825296483bbfa33c1ee4809b8754ab1')
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
        expect_equal(nrow(boot6_dist$data), 240)
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
        expect_equal(nrow(boot6_means), 6)
        expect_equal(ncol(boot6_means), 2)
        expect_equal(digest(paste(sort(colnames(boot6_means)), collapse = "")), '35d687b4f0369a9d4e0a6ef74556908e')
        expect_equal(digest(round(as.numeric(boot6_means$mean[1]))), '8eaea8fff4c78a39455fb520153717a1')
    })
    print("Success!")
}

test_2.2 <- function(){
    test_that("boot1000 should have 2 columns, named replicate and age", {
        expect_equal(digest(paste(sort(colnames(boot1000)), collapse = "")), 'f4f0b2eff0a0eb0d22ac4df99afd13b7')
    })
    test_that("boot1000 have 40000 rows (1000 times the number of observations in one_sample)", {
        expect_equal(nrow(boot1000), 40000)
    })
    test_that("boot1000 does not have the correct values in the age column", {
        expect_equal(digest(round(as.numeric(sum(boot1000$age)))), '3aa5cb2f6d321ad13368310a9c16be2a')
    })
    print("Success!")
}

test_2.3 <- function(){
    test_that('boot1000_means should have 2 columns (named replicate & mean), and 1000 rows.', {
        expect_equal(nrow(boot1000_means), 1000)
        expect_equal(ncol(boot1000_means), 2)
        expect_equal(digest(paste(sort(colnames(boot1000_means)), collapse = "")), '35d687b4f0369a9d4e0a6ef74556908e')
        expect_equal(digest(round(as.numeric(boot1000_means$mean[1]))), '8eaea8fff4c78a39455fb520153717a1')
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
        expect_equal(nrow(boot_est_dist$data), 1000)
        expect_equal(digest(round(as.numeric(sum(boot_est_dist$data)))), '90f7bbe637eee5dce16f172d8673e98c')
    })
   test_that('Labels on the x axis should be descriptive. The plot should have a descriptive title.', {
        expect_false((labels$x) == 'age')
        expect_false(is.null(labels$title))
    })
    print("Success!")
}

test_2.5 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.5), '81949aed6f8e18b150efa97ff46a6fc3')
    })
    print("Success!")
}

test_2.6 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.6), '96c24a598c808db5ff9c1aa505c6aa15')
    })
    print("Success!")
}

test_2.7 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.7), '81949aed6f8e18b150efa97ff46a6fc3')
    })
    print("Success!")
}

test_2.8 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.8), '96c24a598c808db5ff9c1aa505c6aa15')
    })
    print("Success!")
}

test_2.9 <- function(){
    test_that("Answer is incorrect", {
        expect_equal(digest(answer2.9), '81949aed6f8e18b150efa97ff46a6fc3')
    })
    print("Success!")
}