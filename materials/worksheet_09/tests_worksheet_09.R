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
    xint1 = xint + 1
    if (abs(xint - x) < abs(xint1 - x)){
        return(xint)
    }
    else {
        return(xint1)
    }
}

test_1.0 <- function() {
    test_that('Did not create an object named answer1.0', {
        expect_true(exists('answer1.0'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.0), '3a5505c06543876fe45598b5e5e5195d')
    })
    print("Success!")
}

test_1.1 <- function() {
    test_that('Did not create an object named answer1.1', {
        expect_true(exists('answer1.1'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.1), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_1.2 <- function() {
    test_that('Did not create an object named answer1.2', {
        expect_true(exists('answer1.2'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.2), '75f1160e72554f4270c809f041c7a776')
    })
    print("Success!")
}

test_2.0 <- function() {
    test_that('Did not create an object named answer2.0', {
        expect_true(exists('answer2.0'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(int_round(answer2.0, 2)), '9a6564e67167bff7e7cf99a541a641f1')
    })
    print("Success!")
}

test_2.1 <- function() {
    test_that('Did not create an object named answer2.1', {
        expect_true(exists('answer2.1'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(int_round(answer2.1, 2)), '9a6564e67167bff7e7cf99a541a641f1')
    })
    print("Success!")
}

test_2.2 <- function() {
    test_that('Did not create an object named answer2.2', {
        expect_true(exists('answer2.2'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(int_round(answer2.2, 2)), 'd69e7827ff0b1272336c2136df42c3f0')
    })
    print("Success!")
}

test_2.3 <- function() {
    test_that('Did not create an object named answer2.3', {
        expect_true(exists('answer2.3'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.3), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_3.0 <- function() {
    test_that('Did not create an object named marathon', {
        expect_true(exists("marathon")) 
    })
    test_that('marathon should be a tibble.', {
        expect_true('tbl' %in% class(marathon))
    })
    test_that('marathon does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon), c(929, 13))
    })
    test_that('The marathon tibble does not contain the correct columns.', {
        expect_true("time_hrs" %in% colnames(marathon))
        expect_true("max" %in% colnames(marathon))
    })
    test_that('marathon contains the wrong data', {
        expect_equal(digest(int_round(sum(marathon$max), 0)), 'b64d424699e3efa872a878b15e4615fc')
        expect_equal(digest(int_round(sum(marathon$time_hrs), 0)), '0a386b4fbb992709ee886a69c311a49c')
    })
    print("Success!")
}

test_3.1 <- function() {
    test_that('Did not create an object named marathon_split', {
        expect_true(exists('marathon_split'))
    })
    test_that('marathon_split is not a split object (not a tibble)', {
        expect_true('rsplit' %in% class(marathon_split))
    })
    test_that('marathon_split does not contain marathon data', {
        expect_equal(dim(marathon_split$data), c(929, 13))
        expect_equal(digest(int_round(sum(marathon_split$data$max), 0)), 'b64d424699e3efa872a878b15e4615fc')
        expect_equal(digest(int_round(sum(marathon_split$data$time_hrs), 0)), '0a386b4fbb992709ee886a69c311a49c')
    })
    test_that('Did not create an object named marathon_training', {
        expect_true(exists('marathon_training'))
    })
    test_that('marathon_training is not a tibble', {
        expect_true('tbl' %in% class(marathon_training))
    })
    test_that('marathon_training does not contain 0.75 of the marathon data', {
        expect_equal(dim(marathon_training), c(698, 13))
        expect_equal(digest(int_round(sum(marathon_training$max), 0)), '6e85687c32809edf13dccf228f9f84e9')
        expect_equal(digest(int_round(sum(marathon_training$time_hrs), 0)), '2213c3a0eb86305be22e0ca3b0a773c1')
    })
    test_that('Did not create an object named marathon_testing', {
        expect_true(exists('marathon_testing'))
    })
    test_that('marathon_testing is not a tibble', {
        expect_true('tbl' %in% class(marathon_testing))
    })
    test_that('marathon testing does not contain 0.25 of the marathon data', {
        expect_equal(dim(marathon_testing), c(231, 13))
        expect_equal(digest(int_round(sum(marathon_testing$max), 0)), 'e5032644f10cbf9a251aff4ed126d4af')
        expect_equal(digest(int_round(sum(marathon_testing$time_hrs), 0)), 'ba825ab3d722243760b0700769f9371b')
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
        expect_equal(int_round(nrow(marathon_eda$data), 0), 698)
    })
    print("Success!")
}

test_3.3 <- function() {
    test_that('Did not create an object named lm_spec', {
        expect_true(exists("lm_spec")) 
    })
    test_that('lm_spec is not a linear regression model', {
        expect_true('linear_reg' %in% class(lm_spec))
    })
    test_that('lm_spec does not contain the correct specifications', {
        expect_equal(digest(as.character(lm_spec$mode)), 'b8bdd7015e0d1c6037512fd1396aef1a')
        expect_equal(digest(as.character(lm_spec$engine)), '0995419f6f003f701c545d050292f42d')
    })
    print("Success!")
}

test_3.3.1 <- function() {
    test_that('Did not create an object named lm_recipe', {
        expect_true(exists('lm_recipe'))
    })
    test_that('lm_recipe is not a recipe', {
        expect_true('recipe' %in% class(lm_recipe))
    })
    test_that('lm_recipe does not contain the correct variables', {
        expect_equal(digest(int_round(sum(lm_recipe$template$max), 0)), '6e85687c32809edf13dccf228f9f84e9')
        expect_equal(digest(int_round(sum(lm_recipe$template$time_hrs), 0)), '2213c3a0eb86305be22e0ca3b0a773c1')
    })
    test_that('Did not create an object named lm_fit', {
        expect_true(exists('lm_fit'))
    })
    test_that('lm_fit is not a workflow', {
        expect_true('workflow' %in% class(lm_fit))
    })
    test_that('lm_fit does not contain the correct data', {
        expect_equal(digest(int_round(sum(lm_fit$pre$actions$recipe$recipe$template$max), 0)), '6e85687c32809edf13dccf228f9f84e9')
        expect_equal(digest(int_round(sum(lm_fit$pre$actions$recipe$recipe$template$time_hrs), 0)), '2213c3a0eb86305be22e0ca3b0a773c1')
    })
    test_that('lm_fit coefficients are incorrect', {
        expect_equal(digest(int_round(sum(lm_fit$fit$fit$fit$coefficients), 1)), '80b0ae73fe0e882b0a24973e4e2c8203')
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
    test_that('Did not create an object named lm_test_results', {
        expect_true(exists('lm_test_results'))
    })
    test_that('lm_test_results is not a tibble', {
        expect_true('tbl' %in% class(lm_test_results))
    })
    test_that('lm_test_results does not contain the correct data', {
        expect_equal(dim(lm_test_results), c(3, 3))
        expect_equal(digest(int_round(sum(lm_test_results$.estimate), 1)), 'a86d0670df7fb4f1da7b38943f5ee4e7')
    })
    test_that('Did not create an object named lm_rmspe', {
        expect_true(exists('lm_rmspe'))
    })
    test_that('lm_rmspe is not a numeric', {
        expect_true('numeric' %in% class(lm_rmspe))
    })
    test_that('lm_rmspe is not correct', {
        expect_equal(digest(int_round(lm_rmspe, 1)), '25e6a154090e35101d7678d6f034353a')
    })
    print("Success!")
}

test_3.5.1 <- function() {
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
        expect_equal(int_round(nrow(lm_predictions_test$data), 0), 231)
    })
    print("Success!")
}

test_3.6 <- function() {
    test_that('Did not create an object named answer3.6', {
        expect_true(exists('answer3.6'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.6), '75f1160e72554f4270c809f041c7a776')
    })
    print("Success!")
}

test_3.7 <- function() {
    test_that('Did not create an object named answer3.7', {
        expect_true(exists('answer3.7'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.7), '3a5505c06543876fe45598b5e5e5195d')
    })
    print("Success!")
}

test_3.8.1 <- function() {
    test_that('Did not create an object named answer3.8.1', {
        expect_true(exists('answer3.8.1'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.8.1), '75f1160e72554f4270c809f041c7a776')
    })
    print("Success!")
}
