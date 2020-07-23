library(testthat)
library(digest)

test_1.0 <- function(){
test_that('Did not create an object named fruit_data', {
    expect_true(exists("fruit_data")) 
})
test_that('fruit_data should be a data frame.', {
    expect_true('data.frame' %in% class(fruit_data))
})
test_that('fruit_data does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(fruit_data), c(59, 7))
})
test_that('fruit_data does not contain the correct data.', {
    expect_equal(digest(round(as.numeric(sum(fruit_data$scaled_width)))), '908d1fd10b357ed0ceaaec823abf81bc')
    expect_equal(colnames(fruit_data), c("fruit_label", "fruit_name", "fruit_subtype", "scaled_mass", "scaled_width", "scaled_height", "scaled_color"))
})
print("Success!")
    }

test_1.1 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer1.1), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
})
print("Success!")
    }

test_1.2 <- function(){
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
    expect_equal(digest(round(as.numeric(sum(fruit_data$scaled_mass, na.rm = TRUE)))), 'b01ec9609569aecde321d7dd2674f6e1') # we hid the answer to the test here so you can't see it, but we can still run the test  
})
print("Success!")
    }

test_1.3 <- function(){
    properties <- c(fruit_plot$layers[[1]]$mapping, fruit_plot$mapping)
    labels <- fruit_plot$labels
test_that('Did not create a plot named fruit_plot', {
    expect_true(exists("fruit_plot")) 
})
test_that('scaled_mass should be on the x-axis.', {
    expect_true("scaled_mass" == rlang::get_expr(properties$x))
})
test_that('scaled_color should be on the y-axis.', {
    expect_true("scaled_color" == rlang::get_expr(properties$y))
})
test_that('fruit_name should be mapped to colour', {
    expect_true("fruit_name" == rlang::get_expr(properties$colour))
})
test_that('fruit_plot should be a scatter plot.', {
    expect_true("GeomPoint" %in% c(class(fruit_plot$layers[[1]]$geom)))
})
test_that('Labels on the axes should be descriptive and human readable.', {
    expect_false((labels$y) == 'scaled_color')
    expect_false((labels$x) == 'scaled_mass')
    expect_false((labels$colour) == 'fruit_name')
})
print("Success!")
    }

test_1.4 <- function(){
    properties <- c(fruit_plot_new$layers[[1]]$mapping, fruit_plot_new$mapping)
    labels <- fruit_plot_new$labels
test_that('Did not create a plot named fruit_plot', {
    expect_true(exists("fruit_plot_new")) 
})
test_that('scaled_mass should be on the x-axis.', {
    expect_true("scaled_mass" == rlang::get_expr(properties$x))
})
test_that('scaled_color should be on the y-axis.', {
    expect_true("scaled_color" == rlang::get_expr(properties$y))
})
test_that('fruit_name should be mapped to colour', {
    expect_true("fruit_name" == rlang::get_expr(properties$colour))
})
test_that('fruit_plot_new should be a scatter plot.', {
    expect_true("GeomPoint" %in% c(class(fruit_plot_new$layers[[1]]$geom)))
})
test_that ('There should be a new data point that has a mass and color score of 0.5', {
    expect_true('GeomPoint' %in% class(rlang::get_expr(fruit_plot_new$layers[[2]]$geom)))
})
test_that('Labels on the axes should be descriptive and human readable.', {
    expect_false((labels$y) == 'scaled_color')
    expect_false((labels$x) == 'scaled_mass')
    expect_false((labels$colour) == 'fruit_name')
})
print("Success!")
    }


test_1.6 <- function(){
test_that('Did not create an object named X_train', {
    expect_true(exists("X_train")) 
    })
test_that('X_train should be a data frame.', {
    expect_true('data.frame' %in% class(X_train))
    })
test_that('X_train does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(X_train), c(59, 2))
    })
test_that('X_train does not contain the column(s) scaled_color and/or scaled_mass', {
    expect_true('scaled_color' %in% colnames(X_train))
    expect_true('scaled_mass' %in% colnames(X_train))
    })
test_that('Did not create an object named Y_train', {
    expect_true(exists("Y_train")) 
    })
#test_that('Y_train should be a character.', {
#    expect_true('character' %in% class(Y_train))
#    })
test_that('Y_train is not the correct length.', {
    expect_equal(length(Y_train), 59)
    })
print("Success!")
    }

test_1.7 <- function(){
test_that('method should be knn', {
    expect_equal(as.character(fruit_class$method), 'knn')
    })
test_that('k should be 5', {
    expect_equal(as.numeric(fruit_class$results$k), 5)
    })
test_that('model_knn contains incorrect information.', {
    expect_equal(digest(as.numeric(sum(fruit_class$trainingData$scaled_mass))), '9c46762a9ec19d9658dc07063ead8f30')
    expect_equal(digest(as.numeric(sum(fruit_class$trainingData$scaled_color))), '2a93b1da4bcda1113ef03e891938eac4')
    expect_equal(as.numeric(summary(fruit_class$trainingData$.outcome)[1]), 19)
    })
print("Success!")
    }

test_1.8 <- function(){
test_that('Prediction is incorrect', {
    expect_equal(digest(as.character(fruit_predicted)), '17f79d7a98f732174cc5a86dc56380d6')
})
print("Success!")
    }