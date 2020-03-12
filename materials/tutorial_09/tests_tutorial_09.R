library(testthat)
library(digest)

test_1.0 <- function() {
    test_that('Did not create an object named credit', {
        expect_true(exists("credit"))
    })
    test_that('credit should be a data frame.', {
        expect_true('data.frame' %in% class(credit))
    })
    test_that('credit does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(credit), c(400, 12))
    })
    test_that('The credit data frame is missing columns.', {
        expect_that("Income" %in% colnames(credit), is_true())
        expect_that("Balance" %in% colnames(credit), is_true())
    })
    print("Success!")
}

test_1.1 <- function() {
    test_that('credit should be a data frame.', {
        expect_true('data.frame' %in% class(credit))
    })
    test_that('credit does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(credit), c(400, 3))
    })
    test_that('The credit data frame should not contain the column X1', {
        expect_that("X1" %in% colnames(credit), is_false())
    })
    test_that('The credit data frame is missing columns.', {
        expect_that("Income" %in% colnames(credit), is_true())
        expect_that("Balance" %in% colnames(credit), is_true())
        expect_that("Rating" %in% colnames(credit), is_true())
    })
    print("Success!")
}

test_1.2 <- function() {
    test_that('Did not create an object named X_train', {
        expect_true(exists("X_train")) 
        })
    test_that('X_train should be a data frame.', {
        expect_true('data.frame' %in% class(X_train))
        })
    test_that('X_train does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(X_train), c(241, 2))
        })
    test_that('X_train should not contain the column Balance.', {
        expect_false("Balance" %in% colnames(X_train))
        })
    test_that('Did not create an object named X_test', {
        expect_true(exists("X_test")) 
        })
    test_that('X_test should be a data frame.', {
        expect_true('data.frame' %in% class(X_test))
        })
    test_that('X_test does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(X_test), c(159, 2))
        })
    test_that('X_test should not contain the column Balance', {
        expect_false("Balance" %in% colnames(X_test))
        })
    test_that('Did not create an object named Y_train', {
        expect_true(exists("Y_train")) 
        })
    test_that('Y_train should be a numeric list', {
        expect_true('numeric' %in% class(Y_train))
        })
    test_that('Y_train is not the correct length.', {
        expect_equal(length(Y_train), 241)
        })
    test_that('Did not create an object named Y_test', {
        expect_true(exists("Y_test")) 
        })
    test_that('Y_test should be a numeric list', {
        expect_true('numeric' %in% class(Y_test))
        })
    test_that('Y_test is not the correct length.', {
        expect_equal(length(Y_test), 159)
        })
    print("Success!")
}

test_1.3 <- function() {
    test_that('credit_eda should be a pairwise plot matrix.', {
        expect_true('ggmatrix' %in% c(class(credit_eda)))
    })
    test_that('credit_eda should be using data from the credit data frame.', {
        expect_equal(nrow(credit_eda$data), 241)
    })
    test_that('credit_eda should be using the Balance, Income, Rating, Limit columns.', {
        expect_equal(ncol(credit_eda$data), 3)
    })
    print("Success!")
}

test_1.5 <- function() {
    test_that('Did not create an object named lm_model', {
        expect_true(exists("lm_model"))
        })
    test_that('x in lm_model should be X_train', {
        expect_equal(as.character(lm_model$call$x), 'X_train')
        expect_equal(dim(lm_model$trainingData), c(241, 3))
        expect_true('Income' %in% colnames(lm_model$trainingData) && 'Rating' %in% colnames(lm_model$trainingData))
        })
    test_that('y in lm_model should be Y_train', {
        expect_equal(as.character(lm_model$call$y), 'Y_train')
        })
    test_that('method should be lm', {
        expect_equal(as.character(lm_model$method), 'lm')
        })
    test_that('lm_model should have an intercept', {
        expect_true(lm_model$results$intercept)
        })
    print("Success!")
}

test_1.6 <- function() {
    test_that('Did not create an object named lm_coeffs', {
        expect_true(exists("lm_coeffs"))
    })
    test_that('lm_coeffs should be a data frame.', {
        expect_equal(class(lm_coeffs), 'data.frame')
    })
    test_that('lm_coeffs should have 1 row of 3 different coefficients.', {
        expect_equal(dim(lm_coeffs), c(1, 3))
    })
    test_that('column names should include Intercept', {
        expect_true("Intercept" %in% colnames(lm_coeffs))
    })
    test_that('column names should include Income', {
        expect_true("Income" %in% colnames(lm_coeffs))
    })
    test_that('column names should include Rating', {
        expect_true("Rating" %in% colnames(lm_coeffs))
    })
    print("Success!")
}

test_1.8 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(round(lm_rmse)), '506438c6bb7e021d303ffa42da8c4f3a')
    })
    print("Success!")
}

test_1.9 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(round(lm_rmspe)), 'c2c9156cd03604826326da8eab46a3e1')
    })
    print("Success!")
}