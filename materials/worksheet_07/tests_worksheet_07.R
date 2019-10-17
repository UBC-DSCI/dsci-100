library(testthat)
library(digest)

test_0.1 <- function(){
test_that('Solution is incorrect', {
    expect_that(exists('answer1'), is_true())
    expect_equal(digest(answer1), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.0 <- function(){
test_that('Did not create an object named fruit_data', {
    expect_true(exists("fruit_data")) 
})
test_that('fruit_data does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(fruit_data), c(59, 7))
})
test_that('The fruit_name column in fruit_data should be of class factor.', {
    expect_true(is.factor(fruit_data$fruit_name))
})
test_that('Columns in fruit_data contain incorrect values.', {
    expect_equal(digest(as.numeric(sum(fruit_data$mass, na.rm = TRUE))), '351bee4c830e886e0f3a17749569fb33') # we hid the answer to the test here so you can't see it, but we can still run the test  
})
print("Success!")
    }

test_1.1 <- function(){
test_that('Solution is incorrect', {
    expect_that(exists('answer1.1'), is_true())
    expect_equal(digest(answer1.1), '75f1160e72554f4270c809f041c7a776') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.2 <- function(){
test_that('Did not create an object named fruit_dist_2', {
    expect_true(exists("fruit_dist_2")) 
})
test_that('fruit_dist_2 should be a distance.', {
    expect_true('dist' %in% class(fruit_dist_2))
})
test_that('fruit_dist_2 is incorrect.', {
    expect_equal(digest(fruit_dist_2), 'bc63ef43e7c7349f09aca14a55e2c9c0')
    })
print("Success!")
    }

test_1.3 <- function(){
test_that('Did not create an object named fruit_dist_44', {
    expect_true(exists("fruit_dist_44")) 
})
test_that('fruit_dist_44 should be a distance.', {
    expect_true('dist' %in% class(fruit_dist_44))
})
test_that('fruit_dist_44 is incorrect.', {
    expect_equal(digest(fruit_dist_44), 'd775055ad9a05350f4548e1dbe872297')
    })
print("Success!")
    }


test_1.5 <- function(){
test_that('Did not create an object named fruit_data', {
    expect_true(exists("fruit_data")) 
})
test_that('fruit_data does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(fruit_data), c(59, 11))
})
test_that('The fruit_name column in fruit_data should be of class factor.', {
    expect_true(is.factor(fruit_data$fruit_name))
})
test_that('Columns in fruit_data contain incorrect values.', {
    expect_equal(digest(as.numeric(sum(fruit_data$mass, na.rm = TRUE))), '351bee4c830e886e0f3a17749569fb33') # we hid the answer to the test here so you can't see it, but we can still run the test  
})
test_that('The mass, height, color score, and width columns in fruit_data should be scaled.', {
    expect_equal(fruit_data$scaled_mass, scale(fruit_data$mass, center = TRUE))
    expect_equal(fruit_data$scaled_color_score, scale(fruit_data$color_score, center = TRUE))
    expect_equal(fruit_data$scaled_height, scale(fruit_data$height, center = TRUE))
    expect_equal(fruit_data$scaled_width, scale(fruit_data$width, center = TRUE))
})
print("Success!")
    }

test_1.6 <- function(){
test_that('Did not create an object named distance_44', {
    expect_true(exists("distance_44")) 
})
test_that('Did not create an object named distance_2', {
    expect_true(exists("distance_2")) 
})
test_that('distance_44 should be a distance.', {
    expect_true('dist' %in% class(distance_44))
})
test_that('distance_2 should be a distance.', {
    expect_true('dist' %in% class(distance_2))
})
test_that('distance_44 is incorrect.', {
    expect_equal(digest(distance_44), '8b4aad1050358e59d907ad361c4eb9fb')
})
test_that('distance_2 is incorrect.', {
    expect_equal(digest(distance_2), '5c276c2a08b1d63009a6574aefd55212')
})
print("Success!")
    }

test_2.0 <- function(){
test_that('Did not create an object named training_rows', {
    expect_true(exists("training_rows")) 
})
test_that('training_rows does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(training_rows), c(46, 1))
})
test_that('training_rows should only contain fruit names and should be a matrix', {
    expect_true(is.matrix(training_rows))
})
print("Success!")
    }


test_2.1 <- function(){
test_that('Did not create an object named training_set', {
    expect_true(exists("training_set")) 
})
test_that('training_set should contain all the columns in fruit_data and only the rows in training_rows.', {
    expect_equal(dim(training_set), c(46, 11))
})
test_that('Did not create an object named testing_set', {
    expect_true(exists("testing_set")) 
})
test_that('testing_set should contain all the columns in fruit_data and the rows not in training_rows.', {
    expect_equal(dim(testing_set), c(13, 11))
})
print("Success!")
    }

test_2.2 <- function(){
test_that('Did not create an object named X_simple', {
    expect_true(exists("X_simple")) 
    })
test_that('X_simple should be a data frame.', {
    expect_true('data.frame' %in% class(X_simple))
    })
test_that('X_simple does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(X_simple), c(46, 2))
    })
test_that('X_simple does not contain the column(s) scaled_color_score and/or scaled_mass', {
    expect_true('scaled_color_score' %in% colnames(X_simple))
    expect_true('scaled_mass' %in% colnames(X_simple))
    })
test_that('Did not create an object named Y_fruit', {
    expect_true(exists("Y_fruit")) 
    })
test_that('Y_fruit should be a factor', {
    expect_true('factor' %in% class(Y_fruit))
    })
test_that('Y_fruit is not the correct length.', {
    expect_equal(length(Y_fruit), 46)
    })
print("Success!")
    }

test_2.3 <- function(){
test_that('Did not create an object named ks', {
    expect_true(exists("ks")) 
    })
test_that('ks should be a data frame.', {
    expect_true('data.frame' %in% class(ks))
    })
test_that('ks should contain a vector containing all the odd numbers from 1 to 11.', {
    expect_equal(as.numeric(sum(ks)), 36)
    })
print("Success!")
    }

test_2.4 <- function(){
test_that('Did not create an object named train_control', {
    expect_true(exists("train_control")) 
    })
test_that('method should be cv (for cross-validation)', {
    expect_equal(train_control$method, 'cv')
    })
test_that('train_control should be set up for a 10-fold cross-validation (number should be 10))', {
    expect_equal(train_control$number, 10)
    })
print("Success!")
    }

test_2.5 <- function(){
test_that('Did not create an object named choose_k', {
    expect_true(exists("choose_k")) 
    })
test_that('method should be knn', {
    expect_equal(as.character(choose_k$method), 'knn')
    })
test_that('k should be chosen from ks', {
    expect_equal(as.numeric(sum(choose_k$results$k)), 36)
    })
test_that('choose_k should use 10 fold cross-validation.', {
    expect_equal(choose_k$control$method, 'cv')
    expect_equal(choose_k$control$number, 10)
    })
print("Success!")
    }


test_2.6 <- function(){
    properties <- c(choose_k_plot$layers[[1]]$mapping, choose_k_plot$layers[[2]]$mapping, choose_k_plot$mapping)
    labels <- choose_k_plot$labels
test_that('Did not create a plot named choose_k_plot', {
    expect_true(exists("choose_k_plot")) 
})
test_that('choose_k_plot is not using information from k_accuracies', {
    expect_equal(choose_k_plot$data, k_accuracies) 
})
test_that('k should be on the x-axis.', {
    expect_that("k" == rlang::get_expr(properties$x), is_true())
    })
test_that('Accuracy should be on the y-axis.', {
    expect_that("Accuracy" == rlang::get_expr(properties$y), is_true())
    })
test_that('choose_k_plot should have both a line geometric object and point geometric object.', {
    expect_that('GeomPoint' %in% c(class(rlang::get_expr(choose_k_plot$layers[[1]]$geom)), class(rlang::get_expr(choose_k_plot$layers[[2]]$geom))), is_true())
    expect_that('GeomLine' %in% c(class(rlang::get_expr(choose_k_plot$layers[[1]]$geom)), class(rlang::get_expr(choose_k_plot$layers[[2]]$geom))), is_true())
    })
print("Success!")
    }

test_2.7 <- function(){
test_that('Solution is incorrect', {    
    expect_true(answer2.7 == 3) 
})
print("Success!")
    }

test_2.8 <- function(){
test_that('Solution is incorrect', {    
    expect_true(answer2.8 > 0.9)
    expect_true(answer2.8 < 0.95)
})
print("Success!")
    }

test_3.0 <- function(){
test_that('method should be knn', {
    expect_equal(as.character(simple$method), 'knn')
    })
test_that('k should be 3', {
    expect_equal(as.numeric(simple$results$k), 3)
    })
test_that('simple should not use cross-validation.', {
    expect_equal(simple$control$method, 'boot')
    expect_equal(simple$control$number, 25)
    })
print("Success!")
    }

test_3.1 <- function(){
test_that('training_pred prediction is incorrect.', {
    expect_equal(length(training_pred), 46)
    expect_equal(class(training_pred), 'factor')
    expect_equal(c("apple", "lemon", "mandarin", "orange") %in% levels(training_pred), c(TRUE, TRUE, TRUE, TRUE))
    expect_equal(length(training_pred[training_pred == "apple"]), 15)
    expect_equal(length(training_pred[training_pred == "mandarin"]), 4)
    expect_equal(length(training_pred[training_pred == "orange"]), 14)
    expect_equal(length(training_pred[training_pred == "lemon"]), 13)
})
print("Success!")
    }

test_3.2 <- function(){
test_that('Solution is incorrect', {
    expect_equal(class(training_results), 'confusionMatrix')
    expect_equal(c("apple", "lemon", "mandarin", "orange") %in% colnames(training_results$table), c(TRUE, TRUE, TRUE, TRUE))
    expect_equal(round(training_results$overall[[1]], 2), 0.93)
})
print("Success!")
    }

test_3.3 <- function(){
test_that('Solution is incorrect', {
    expect_true(answer3.3 > 0.92)
    expect_true(answer3.3 < 0.94)
})
print("Success!")
    }



test_4.0 <- function(){
test_that('Did not create an object named X_complex', {
    expect_true(exists("X_complex")) 
    })
test_that('X_complex should be a data frame.', {
    expect_true('data.frame' %in% class(X_complex))
})
test_that('X_complex does not contain the correct columns.', {
    expect_that("scaled_height" %in% colnames(X_complex), is_true())
    expect_that("scaled_width" %in% colnames(X_complex), is_true())
    expect_that("scaled_mass" %in% colnames(X_complex), is_true())
    expect_that("scaled_color_score" %in% colnames(X_complex), is_true())
})
test_that('Did not create an object named complex', {
    expect_true(exists("complex")) 
    })
test_that('x in complex should be X_complex', {
    expect_equal(as.character(complex$call$x), 'X_complex')
    })
test_that('method should be knn', {
    expect_equal(as.character(complex$method), 'knn')
    })
test_that('k should be chosen from ks', {
    expect_equal(as.numeric(sum(complex$results$k)), 36)
    })
test_that('complex should use 10 fold cross-validation.', {
    expect_equal(complex$control$method, 'cv')
    expect_equal(complex$control$number, 10)
    })
print("Success!")
    }

test_4.1 <- function(){
test_that('Did not create an object named k_accuracies_again', {
    expect_true(exists("k_accuracies_again")) 
})
test_that('k_accuracies_again contains incorrect information.', {
    expect_equal(k_accuracies_again, complex$results %>% select(k, Accuracy))
})
test_that('Did not create a plot named choose_k_again_plot', {
    expect_true(exists("choose_k_again_plot")) 
})
test_that('choose_k_again_plot is not using information from k_accuracies_again', {
    expect_equal(choose_k_again_plot$data, k_accuracies_again) 
})
test_that('k should be on the x-axis.', {
    expect_that("k" %in% c(rlang::get_expr(choose_k_again_plot$mapping$x),rlang::get_expr(choose_k_again_plot$layers[[1]]$mapping$x)), is_true())
    })
test_that('Accuracy should be on the y-axis.', {
    expect_that("Accuracy" %in% c(rlang::get_expr(choose_k_again_plot$mapping$y), rlang::get_expr(choose_k_again_plot$layers[[1]]$mapping$y)) , is_true())
    })
test_that('choose_k_again_plot should be a scatter plot and a line plot.', {
    expect_that('GeomPoint' %in% c(class(rlang::get_expr(choose_k_again_plot$layers[[1]]$geom)), class(rlang::get_expr(choose_k_again_plot$layers[[2]]$geom))), is_true())
    expect_that('GeomLine' %in% c(class(rlang::get_expr(choose_k_again_plot$layers[[1]]$geom)), class(rlang::get_expr(choose_k_again_plot$layers[[2]]$geom))), is_true())
    })
print("Success!")
    }

test_4.2 <- function(){
test_that('Solution is incorrect', {
    expect_equal(answer4.2, 5)
})
print("Success!")
    }

test_4.3 <- function(){
test_that('Solution is incorrect', {
    expect_true(answer4.3 > 0.97)
    expect_true(answer4.3 < 1)
})
print("Success!")
    }

test_4.4 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer4.4), '96c24a598c808db5ff9c1aa505c6aa15')
})
print("Success!")
    }


test_5.0 <- function(){
test_that('Did not create an object named final_k', {
    expect_true(exists("final_k")) 
    })
test_that('final_k should be a data frame.', {
    expect_true('data.frame' %in% class(final_k))
})
test_that('Did not create an object named final_classifier', {
    expect_true(exists("final_classifier")) 
    })
test_that('x in the final_classifier should be X_complex', {
    expect_equal(as.character(final_classifier$call$x), 'X_complex')
    })
test_that('method should be knn', {
    expect_equal(as.character(final_classifier$method), 'knn')
})
test_that('k should be 5', {
    expect_equal(as.numeric(sum(final_classifier$results$k)), 5)
})
test_that('final_classifier should not be using cross-validation.', {
    expect_equal(final_classifier$control$method, 'boot')
    expect_equal(final_classifier$control$number, 25)
})
print("Success!")
    }

test_5.1 <- function(){
test_that('Did not create an object named X_test', {
    expect_true(exists("X_test")) 
    })
test_that('X_test should be a data frame.', {
    expect_true('data.frame' %in% class(X_test))
    })
test_that('X_test does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(X_test), c(13, 4))
    })
test_that('X_test does not contain the column(s) scaled_mass, scaled_width, scaled_height, and/or scaled_color_score', {
    expect_true('scaled_color_score' %in% colnames(X_test))
    expect_true('scaled_mass' %in% colnames(X_test))
    expect_true('scaled_width' %in% colnames(X_test))
    expect_true('scaled_height' %in% colnames(X_test))
    })
test_that('Did not create an object named Y_test', {
    expect_true(exists("Y_test")) 
    })
test_that('Y_fruit should be a factor', {
    expect_true('factor' %in% class(Y_fruit))
    })
test_that('Y_fruit is not the correct length.', {
    expect_equal(length(Y_test), 13)
    })
test_that('test_results should be a confusion matrix.', {
    expect_equal(class(test_results), 'confusionMatrix')
    })
test_that('test_results does not contain correct information.', {
    expect_equal(c("apple", "lemon", "mandarin", "orange") %in% colnames(test_results$table), c(TRUE, TRUE, TRUE, TRUE))
    }) 
test_that('prediction accuracy is not correct', {
    expect_equal(round(test_results$overall[[1]], 2), 0.62)
    })
print("Success!")
    }

test_5.2 <- function(){
test_that('Solution is incorrect', {
    expect_true(answer5.2 > 0.59)
    expect_true(answer5.2 < 0.65)
})
print("Success!")
    }