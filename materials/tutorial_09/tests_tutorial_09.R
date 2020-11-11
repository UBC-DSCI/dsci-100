# +
library(testthat)
library(digest)
library(rlang)

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
# -

test_1.0 <- function() {
    test_that('Did not create an object named credit', {
        expect_true(exists("credit"))
    })
    test_that('credit should be a tibble.', {
        expect_true('tbl' %in% class(credit))
    })
    test_that('credit does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(credit), c(400, 12))
    })
    test_that('The credit tibble is missing columns.', {
        expect_true("Income" %in% colnames(credit))
        expect_true("Balance" %in% colnames(credit))
    })
    test_that('credit does not contain the correct data.', {
        expect_equal(digest(int_round(sum(credit$Income), 2)), '7b41cc2ef140f2cfb4b6eb86ccebf416')
        expect_equal(digest(int_round(sum(credit$Limit), 2)), '1bc8a53a9b0cc2ea3cf99f2306872029')
    })
    print("Success!")
}

test_1.1 <- function() {
    test_that('credit should be a tibble.', {
        expect_true('tbl' %in% class(credit))
    })
    test_that('credit does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(credit), c(400, 3))
    })
    test_that('The credit data frame should not contain the column X1', {
        expect_false("X1" %in% colnames(credit))
    })
    test_that('The credit data frame is missing columns.', {
        expect_true("Income" %in% colnames(credit))
        expect_true("Balance" %in% colnames(credit))
        expect_true("Rating" %in% colnames(credit))
    })
    print("Success!")
}

test_1.2 <- function() {
    test_that('Did not create an object called credit_split.', {
        expect_true(exists('credit_split'))
    })
    test_that('credit_split is not an r_split object.', {
        expect_true('rsplit' %in% class(credit_split))
    })
    test_that('Did not create an object called credit_training.', {
        expect_true(exists('credit_training'))
    })
    test_that('credit_training is not a tibble.',{
        expect_true('tbl' %in% class(credit_training))
    })
    test_that('credit_training does not contain 60% of the data.', {
        expect_equal(dim(credit_training), c(241, 3))
        expect_equal(digest(int_round(sum(credit_training$Balance), 2)), '273157f18270727a00bfb2d4bff79903')
        expect_equal(digest(int_round(sum(credit_training$Income), 2)), 'b9f9e7fe1b89fc1fc7f54425e5688322')
        expect_equal(digest(int_round(sum(credit_training$Rating), 2)), 'd34e38964a1d4ddb00ddd7f6118c5fc6')
    })
    test_that('Did not create an object called credit_testing.', {
        expect_true(exists('credit_testing'))
    })
    test_that('credit_testing is not a tibble.', {
        expect_true('tbl' %in% class(credit_training))
    })
    test_that('credit_testing does not contain the remaining 40% of the data.', {
        expect_equal(dim(credit_testing), c(159, 3))
        expect_equal(digest(int_round(sum(credit_testing$Balance), 2)), 'b0fe1ffce8158f92072c0c27ad1f181f')
        expect_equal(digest(int_round(sum(credit_testing$Income), 2)), '581f089d90262775f263a2eb16c22480')
        expect_equal(digest(int_round(sum(credit_testing$Rating), 2)), '26a325d7669edb2146d4a17935fe2636')
    })
    print("Success!")
}

test_1.3 <- function() {
    test_that('credit_eda should be a pairwise plot matrix.', {
        expect_true('ggmatrix' %in% c(class(credit_eda)))
    })
    test_that('credit_eda should be using data from the credit data frame.', {
        expect_equal(int_round(nrow(credit_eda$data), 0), 241)
    })
    test_that('credit_eda should be using the Balance, Income, Rating, Limit columns.', {
        expect_equal(int_round(ncol(credit_eda$data), 0), 3)
    })
    print("Success!")
}

test_1.4 <- function() {
    test_that('Did not create an object called answer1.4.', {
        expect_true(exists('answer1.4'))
    })
    test_that('Solution is incorrect.',{
        expect_equal(digest(answer1.4), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_1.5 <- function() {
    test_that('Did not create an object called lm_spec.', {
        expect_true(exists('lm_spec'))
    })
    test_that('lm_spec should be a linear regression model specification.', {
        expect_true('linear_reg' %in% class(lm_spec))
    })
    test_that('Did not create an object called credit_recipe.', {
        expect_true(exists('credit_recipe'))
    })
    test_that('credit_recipe should be a recipe.', {
        expect_true('recipe' %in% class(credit_recipe))
    })
    test_that('credit_recipe does not contain the training data.', {
        expect_equal(dim(credit_recipe$template), c(241, 3))
        expect_equal(digest(int_round(sum(credit_recipe$template$Income), 2)), 'b9f9e7fe1b89fc1fc7f54425e5688322')
        expect_equal(digest(int_round(sum(credit_recipe$template$Rating), 2)), 'd34e38964a1d4ddb00ddd7f6118c5fc6')
        expect_equal(digest(int_round(sum(credit_recipe$template$Balance), 2)), '273157f18270727a00bfb2d4bff79903')
    })
    print("Success!")    
}

test_1.6 <- function() {
    test_that('Did not create an object called credit_fit.', {
        expect_true(exists('credit_fit')) 
    })
    test_that('credit_fit should be a workflow.', {
        expect_true('workflow' %in% class(credit_fit))
    })
    test_that('credit_fit does not contain the training data.', {
        expect_equal(digest(int_round(sum(credit_fit$pre$mold$predictors$Income), 2)), 'b9f9e7fe1b89fc1fc7f54425e5688322')
        expect_equal(digest(int_round(sum(credit_fit$pre$mold$predictors$Rating), 2)), 'd34e38964a1d4ddb00ddd7f6118c5fc6')
    })
    print("Success!")
}

test_1.7 <- function(){
    test_that('Did not create an object called answer1.7.')
        expect_true(exists('answer1.7'))
    test_that('Solution is incorrect.', {
        expect_equal(digest(answer1.7), '75f1160e72554f4270c809f041c7a776')
    })
}

test_1.8 <- function() {
    test_that('Solution is incorrect.', {
        expect_equal(digest(int_round(lm_rmse, 2)), 'fc3357631f940d71d5027270ad54a2cf')
    })
    print("Success!")
}

test_1.9 <- function() {
    test_that('Solution is incorrect.', {
        expect_equal(digest(int_round(lm_rmspe, 2)), 'ec726f8469b8d46ed8231dcbcca420e2')
    })
    print("Success!")
}

test_1.9.2 <- function(){
    test_that('Did not create an object called answer1.9.2', {
        expect_true(exists('answer1.9.2'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.9.2), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}
