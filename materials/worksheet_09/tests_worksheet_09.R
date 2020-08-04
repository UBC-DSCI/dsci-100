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
        expect_that(digest(round(answer2.0, 1)) %in% c('3c3b9d75cc0e8cfcc29f40abd17afe8a', '03b3b7e9967823ca8ae75c138a3aa39c', 'af04a6f39588915a4dcac626c46434de'), is_true())
    })
    print("Success!")
}

test_2.1 <- function() {
    test_that('Solution is incorrect', {
        expect_that(digest(round(answer2.1, 1)) %in% c('3c3b9d75cc0e8cfcc29f40abd17afe8a', '03b3b7e9967823ca8ae75c138a3aa39c', 'af04a6f39588915a4dcac626c46434de'), is_true())
    })
    print("Success!")
}

test_2.2 <- function() {
    test_that('Solution is incorrect', {
        expect_that(digest(round(answer2.2, 1)) %in% c('f23662d0838c244acb308b71749ac22e', '6a8e65e0821e8011c0f04d886dce9323', 'e4de8af39ae8ed8bf8976830f6c8e989'), is_true())
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
    test_that('marathon should be a tibble.', {
        expect_true('tbl' %in% class(marathon))
    })
    test_that('marathon does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon), c(929, 13))
    })
    test_that('The marathon data frame is missing columns.', {
        expect_true("time_hrs" %in% colnames(marathon))
        expect_true("max" %in% colnames(marathon))
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
    properties <- c(marathon_eda$layers[[1]]$mapping, marathon_eda$mapping)
    labels <- marathon_eda$labels
    layers <- marathon_eda$layers[[1]]
    test_that('Did not create a plot named marathon_eda', {
        expect_true(exists("marathon_eda")) 
    })
    test_that('max should be on the x-axis.', {
        expect_true("max" %in% c(rlang::get_expr(properties$x)))
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_true("time_hrs" %in% c(rlang::get_expr(properties$y)))
    })
    test_that('marathon_eda should be a scatter plot.', {
        expect_equal(digest(class(rlang::get_expr(layers$geom))[1]), '911e5b9debfb523f25ad2ccc01a4b2dd')
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false((labels$y) == 'time_hrs')
        expect_false((labels$x) == 'max')
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
    properties <- c(lm_predictions$layers[[1]]$mapping, lm_predictions$mapping)
    labels <- lm_predictions$labels
    layers <- c(lm_predictions$layers[[1]], lm_predictions$layers[[2]])
    layers2 <- c(lm_predictions$layers[[2]], lm_predictions$layers[[1]])
    test_that('Did not create a plot named lm_predictions', {
        expect_true(exists("lm_predictions")) 
    })
    test_that('max should be on the x-axis.', {
        expect_true("max" %in% c(rlang::get_expr(properties$x)))
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_true("time_hrs" %in% c(rlang::get_expr(properties$y)))
    })
    test_that('lm_predictions should be a scatter plot.', {
        expect_true('GeomPoint' %in% c(class(rlang::get_expr(lm_predictions$layers[[1]]$geom)),
                                       class(rlang::get_expr(lm_predictions$layers[[2]]$geom))))

    })
    test_that('lm_predictions should have a best fit line using a linear regression model.', {
        expect_true('GeomSmooth' %in% c(class(rlang::get_expr(lm_predictions$layers[[2]]$geom)), 
                                        class(rlang::get_expr(lm_predictions$layers[[1]]$geom))))
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false((labels$y) == 'time_hrs')
        expect_false((labels$x) == 'max')
    })
    print("Success!")
}

test_3.5 <- function() {
    test_that('lm_rmse is incorrect', {
        expect_equal(digest(round(lm_rmse, 3)), '32db709a07227d2d79e251ce8640fb6f')
    })
    print("Success!")
}

test_3.6 <- function() {
    test_that('lm_rmspe is incorrect', {
        expect_equal(digest(round(lm_rmspe, 3)), '28944cb9edc136e1368f62996d6750d6')
    })
    print("Success!")
}

test_3.6.1 <- function() {
    properties <- c(lm_predictions_test$mapping, lm_predictions_test$layers[[1]]$mapping)
    labels <- lm_predictions_test$labels
    layers <- lm_predictions_test$layers[[1]]
    test_that('Did not create a plot named lm_predictions_test', {
        expect_true(exists("lm_predictions_test")) 
    })
    test_that('max should be on the x-axis.', {
        expect_true("max" %in% c(rlang::get_expr(properties$x)))
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_true("time_hrs" %in% c(rlang::get_expr(properties$y)))
    })
    test_that('lm_predictions_test should be a scatter plot.', {
        expect_equal(digest(class(rlang::get_expr(layers$geom))[1]), '911e5b9debfb523f25ad2ccc01a4b2dd')
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false((labels$y) == 'time_hrs')
        expect_false((labels$x) == 'max')
    })
    test_that('Only the testing data set should be used to create the plot', {
        expect_equal(nrow(lm_predictions_test$data), 231)
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

test_3.9.1 <- function() {
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.9.1), '75f1160e72554f4270c809f041c7a776')
    })
    print("Success!")
}
