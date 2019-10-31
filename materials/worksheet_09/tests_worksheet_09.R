library(testthat)
library(digest)

test_1.0 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.0), '3a5505c06543876fe45598b5e5e5195d')
    })
    print("Success!")
}

test_1.1 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.1), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_1.2 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.2), '75f1160e72554f4270c809f041c7a776')
    })
    print("Success!")
}

test_2.0 <- function() {
    test_that('Solution is incorrect', {
        expect_that(digest(round(answer2.0, 2)) %in% c('75dc8b7b8724a54d1fba4cc109438cfb', '4ec4e6dd2f7793c5d566f50026f262e9', '0d8b1b67a03fc038058a25213d5e9778'), is_true())
    })
    print("Success!")
}

test_2.1 <- function() {
    test_that('Solution is incorrect', {
        expect_that(digest(round(answer2.1, 2)) %in% c('62233ed4e6655a993784e4c0886c4550', '1825ac9b036d540ac34abdd1ecb7fc21', '354964e94313ac16ac091669a785eb4f'), is_true())
    })
    print("Success!")
}

test_2.2 <- function() {
    test_that('Solution is incorrect', {
        expect_that(digest(round(answer2.2, 2)) %in% c('522dbf08f17812fee06f0991cf0481af', 'ee48059132b8cdd8f1a1d9abbdaead78', '37cd4e5174c65a7196eae5fed7c0a61e', '4d308066a8d7253145df19089a026b9e'), is_true())
    })
    print("Success!")
}

test_2.3 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.3), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_3.0 <- function() {
    test_that('Did not create an object named marathon', {
        expect_true(exists("marathon")) 
    })
    test_that('marathon should be a data frame.', {
        expect_true('data.frame' %in% class(marathon))
    })
    test_that('marathon does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon), c(929, 13))
    })
    test_that('The marathon data frame is missing columns.', {
        expect_that("time_hrs" %in% colnames(marathon), is_true())
        expect_that("max" %in% colnames(marathon), is_true())
    })
    print("Success!")
}

test_3.1 <- function() {
    test_that('Did not create an object named X_train', {
        expect_true(exists("X_train")) 
    })
    test_that('X_train should be a data frame.', {
        expect_true('data.frame' %in% class(X_train))
    })
    test_that('X_train does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(X_train), c(698, 1))
    })
    test_that('X_train does not contain the column max', {
        expect_true('max' %in% colnames(X_train))
    })
    test_that('Did not create an object named X_test', {
        expect_true(exists("X_test")) 
    })
    test_that('X_test should be a data frame.', {
        expect_true('data.frame' %in% class(X_test))
    })
    test_that('X_test does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(X_test), c(231, 1))
    })
    test_that('X_test does not contain the column max', {
        expect_true('max' %in% colnames(X_test))
    })
    test_that('Did not create an object named Y_train', {
        expect_true(exists("Y_train")) 
    })
    test_that('Y_train should be a numeric vector', {
        expect_true('numeric' %in% class(Y_train))
    })
    test_that('Y_train is not the correct length.', {
        expect_equal(length(Y_train), 698)
    })
    test_that('Did not create an object named Y_test', {
        expect_true(exists("Y_test")) 
    })
    test_that('Y_test should be a numeric vector', {
        expect_true('numeric' %in% class(Y_test))
    })
    test_that('Y_test is not the correct length.', {
        expect_equal(length(Y_test), 231)
    })
    print("Success!")
}

test_3.2 <- function() {
    test_that('Did not create a plot named marathon_eda', {
        expect_true(exists("marathon_eda")) 
    })
    test_that('max should be on the x-axis.', {
        expect_that("max" %in% c(rlang::get_expr(marathon_eda$mapping$x),rlang::get_expr(marathon_eda$layers[[1]]$mapping$x)), is_true())
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_that("time_hrs" %in% c(rlang::get_expr(marathon_eda$mapping$y), rlang::get_expr(marathon_eda$layers[[1]]$mapping$y)) , is_true())
    })
    test_that('marathon_eda should be a scatter plot.', {
        expect_equal(digest(class(rlang::get_expr(marathon_eda$layers[[1]]$geom))[1]), '911e5b9debfb523f25ad2ccc01a4b2dd')
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((marathon_eda$labels$y) == 'time_hrs', is_false())
        expect_that((marathon_eda$labels$x) == 'max', is_false())
    })
    test_that('Only the training data set should be used to create the plot', {
        expect_equal(nrow(marathon_eda$data), 698)
    })
    print("Success!")
}

test_3.3 <- function() {
    test_that('Did not create an object named lm_model', {
        expect_true(exists("lm_model")) 
    })
    test_that('x in lm_model should be X_train', {
        expect_equal(as.character(lm_model$call$x), 'X_train')
        expect_equal(dim(lm_model$trainingData), c(698, 2))
    })
    test_that('y in lm_model should be Y_train', {
        expect_equal(as.character(lm_model$call$y), 'Y_train')
        expect_equal(dim(lm_model$trainingData), c(698, 2))
    })
    test_that('method should be lm', {
        expect_equal(as.character(lm_model$method), 'lm')
    })
print("Success!")
}

test_3.4 <- function() {
    test_that('Did not create a plot named lm_predictions', {
        expect_true(exists("lm_predictions")) 
    })
    test_that('max should be on the x-axis.', {
        expect_that("max" %in% c(rlang::get_expr(lm_predictions$mapping$x),rlang::get_expr(lm_predictions$layers[[1]]$mapping$x)), is_true())
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_that("time_hrs" %in% c(rlang::get_expr(lm_predictions$mapping$y), rlang::get_expr(lm_predictions$layers[[1]]$mapping$y)) , is_true())
    })
    test_that('lm_predictions should be a scatter plot.', {
        expect_true('GeomPoint' %in% c(class(rlang::get_expr(lm_predictions$layers[[1]]$geom)), class(rlang::get_expr(lm_predictions$layers[[2]]$geom))))
    })
    test_that('lm_predictions should have a best fit line using a linear regression model.', {
        expect_true('GeomSmooth' %in% c(class(rlang::get_expr(lm_predictions$layers[[2]]$geom)), class(rlang::get_expr(lm_predictions$layers[[1]]$geom))))
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((lm_predictions$labels$y) == 'time_hrs', is_false())
        expect_that((lm_predictions$labels$x) == 'max', is_false())
    })
    print("Success!")
}

test_3.5 <- function() {
    test_that('lm_rmse is incorrect', {
        expect_equal(digest(round(lm_rmse, 3)), '788b714543df6e5fa9954cdefb85fde8')
    })
    print("Success!")
}

test_3.6 <- function() {
    test_that('lm_rmspe is incorrect', {
        expect_equal(digest(round(lm_rmspe, 3)), '53a185c37dade264b27beeead0c1e823')
    })
    print("Success!")
}

test_3.61 <- function() {
    test_that('Did not create a plot named lm_predictions', {
        expect_true(exists("lm_predictions")) 
    })
    test_that('max should be on the x-axis.', {
        expect_that("max" %in% c(rlang::get_expr(lm_predictions$mapping$x),rlang::get_expr(lm_predictions$layers[[1]]$mapping$x)), is_true())
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_that("time_hrs" %in% c(rlang::get_expr(lm_predictions$mapping$y), rlang::get_expr(lm_predictions$layers[[1]]$mapping$y)) , is_true())
    })
    test_that('lm_predictions should be a scatter plot.', {
        expect_equal(digest(class(rlang::get_expr(lm_predictions$layers[[1]]$geom))[1]), '911e5b9debfb523f25ad2ccc01a4b2dd')
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((lm_predictions$labels$y) == 'time_hrs', is_false())
        expect_that((lm_predictions$labels$x) == 'max', is_false())
    })
    test_that('Only the testing data set should be used to create the plot', {
        expect_equal(nrow(lm_predictions$data), 231)
    })
    print("Success!")
}

test_3.7 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.7), '75f1160e72554f4270c809f041c7a776')
    })
    print("Success!")
}

test_3.8 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.8), '3a5505c06543876fe45598b5e5e5195d')
    })
    print("Success!")
}


