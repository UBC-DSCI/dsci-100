library(testthat)
library(digest)

test_0.1 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer0.1), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }
    
test_0.2 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer0.2), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }


test_1.0 <- function(){
test_that('Did not create an object named cancer', {
    expect_true(exists("cancer")) 
})
test_that('cancer should be a data frame.', {
    expect_true('data.frame' %in% class(cancer))
})
test_that('cancer does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(cancer), c(569, 12))
})
test_that('cancer does not contain the correct data.', {
    expect_equal(digest(round(as.numeric(sum(cancer$Area)))), 'b01ec9609569aecde321d7dd2674f6e1')
    expect_equal(colnames(cancer), c("ID", "Class", "Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity", "Concave_points", "Symmetry", "Fractal_dimension"))
})
test_that('read.csv() instead of read_csv() function is used.', {
    expect_true(class(cancer$Class) == 'character')
})
print("Success!")
    }

test_1.1 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer1.1), 'd2a90307aac5ae8d0ef58e2fe730d38b') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.2 <- function(){
    properties <- c(cancer_plot$layers[[1]]$mapping, cancer_plot$mapping)
    labels <- cancer_plot$labels
test_that('Did not create a plot named cancer_plot', {
    expect_true(exists("cancer_plot")) 
})
test_that('Symmetry should be on the x-axis.', {
    expect_true("Symmetry" == rlang::get_expr(properties$x))
    })
test_that('Radius should be on the y-axis.', {
    expect_true("Radius" == rlang::get_expr(properties$y))
    })
test_that('cancer_plot should be a scatter plot.', {
    expect_true("GeomPoint" %in% class(cancer_plot$layers[[1]]$geom))
    })
test_that('cancer_plot should map Class to colour.', {
    expect_true(digest(rlang::get_expr(properties$colour)) %in% c('a4abb3d43fde633563dd1f5c3ea31f31', 'f9e884084b84794d762a535f3facec85'))
    })
print("Success!")
    }

test_1.3 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer1.3), '891e8a631267b478c03e25594808709d') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.4 <- function(){
test_that('xa is incorrect.', {
    expect_equal(digest(round(as.numeric(xa))), 'e5b57f323c7b3719bbaaf9f96b260d39')
    })
test_that('ya is incorrect.', {
    expect_equal(digest(round(as.numeric(ya))), 'db8e490a925a60e62212cefc7674ca02')
    })
test_that('xb is incorrect.', {
    expect_equal(digest(round(as.numeric(xb))), 'b01ec9609569aecde321d7dd2674f6e1')
    })
test_that('yb is incorrect.', {
    expect_equal(digest(round(as.numeric(yb))), 'db8e490a925a60e62212cefc7674ca02')
    })
print("Success!")
    }

test_1.5 <- function(){
test_that('answer1.5 is incorrect', {    
    expect_equal(digest(round(as.numeric(answer1.5))), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.6 <- function(){
test_that('zb is incorrect.', {
    expect_equal(digest(round(as.numeric(zb))), 'b01ec9609569aecde321d7dd2674f6e1')
    })
test_that('za is incorrect.', {
    expect_equal(digest(round(as.numeric(za))), 'db8e490a925a60e62212cefc7674ca02')
    })
print("Success!")
    }

test_1.7 <- function(){
test_that('answer1.7 is incorrect', {    
    expect_equal(digest(round(as.numeric(answer1.7))), 'dbc09cba9fe2583fb01d63c70e1555a8') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }



test_1.8 <- function(){
  test_that('point_a is incorrect.', {
    expect_equal(digest(round(as.numeric(sum(point_a)))), 'db8e490a925a60e62212cefc7674ca02')
    })
test_that('point_b is incorrect.', {
    expect_equal(digest(round(as.numeric(sum(point_b)))), 'b01ec9609569aecde321d7dd2674f6e1')
    })
print("Success!")
    }

test_1.9 <- function(){
test_that('difference is incorrect', {    
    expect_equal(digest(round(as.numeric(sum(difference)))), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.10 <- function(){
test_that('dif_square is incorrect', {    
    expect_equal(digest(round(as.numeric(sum(dif_square)))), '5e338704a8e069ebd8b38ca71991cf94') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.10.1 <- function(){
test_that('dif_sum is incorrect', {    
    expect_equal(digest(round(as.numeric(dif_sum))), '5e338704a8e069ebd8b38ca71991cf94') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
}

test_1.10.2 <- function(){
test_that('root_dif_sum is incorrect', {    
    expect_equal(digest(round(as.numeric(root_dif_sum))), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
}

test_1.10.3 <- function(){
test_that('dist_cancer_two_rows is incorrect', {    
    expect_equal(digest(round(as.numeric(dist_cancer_two_rows))), 'dbc09cba9fe2583fb01d63c70e1555a8') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
}


test_1.10.4 <- function(){
test_that('Solution is incorrect', {    
    expect_equal(digest(answer1.10.4), '05ca18b596514af73f6880309a21b5dd') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
}

test_2.0.0 <- function(){
test_that('Did not create an object named small_sample', {
    expect_true(exists("small_sample")) 
    })
test_that('small_sample should be a data frame.', {
    expect_true('data.frame' %in% class(small_sample))
    })
test_that('small_sample does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(small_sample), c(5, 3))
    })
test_that('small_sample does not contain the correct columns.', {
    expect_true('Symmetry' %in% colnames(small_sample))
    expect_true('Radius' %in% colnames(small_sample))
    expect_true('Class' %in% colnames(small_sample))
    })
print("Success!")
    }


test_2.0.1 <- function(){
    properties <- c(small_sample_plot$layers[[1]]$mapping, small_sample_plot$mapping)
    labels <- small_sample_plot$labels
test_that('Did not create a plot named small_sample_plot', {
    expect_true(exists("small_sample_plot")) 
    })
test_that('Did not use small_sample data to create small_sample_plot', {
    expect_equal(digest(small_sample_plot$data$Symmetry), 'f4c490a7955304fbdeb905214ea0f3d3')
    })
test_that('Symmetry should be on the x-axis.', {
    expect_true("Symmetry" == rlang::get_expr(properties$x))
    })
test_that('Radius should be on the y-axis.', {
    expect_true("Radius" == rlang::get_expr(properties$y))
    })
test_that('small_sample_plot should be a scatter plot.', {
    expect_true("GeomPoint" %in% c(class(small_sample_plot$layers[[1]]$geom)))
    })
test_that('small_sample_plot should map Benign / Malignant to colour.', {
    expect_true("Class" == rlang::get_expr(properties$colour))
    })
print("Success!")
    }

test_2.1 <- function(){
test_that('Did not create an object named newData', {
    expect_true(exists("newData")) 
    })
test_that('newData should be a data frame.', {
    expect_true('data.frame' %in% class(newData))
    })
test_that('The last row of the Class column should be unknown.', {
    expect_equal((newData$Class[6]), 'unknown')
    })
test_that('newData does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(newData), c(6, 3))
    })
test_that('small_sample does not contain the correct data.', {
    expect_equal(digest(round(sum(as.numeric(newData$Radius)))), '6717f2823d3202449301145073ab8719')
    expect_equal(digest(round(sum(as.numeric(newData$Symmetry)))), 'db8e490a925a60e62212cefc7674ca02')
    })
print("Success!")
    }

test_2.2 <- function(){
test_that('Did not create an object named dist_matrix', {
    expect_true(exists("dist_matrix")) 
    })
test_that('dist_matrix should be a matrix.', {
    expect_true('matrix' %in% class(dist_matrix))
    })
test_that('dist_matrix does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(dist_matrix), c(6, 6))
    })
test_that('dist_matrix does not contain the correct data.', {
    expect_equal(digest(round(as.numeric(sum(dist_matrix[1, ])))), '4a5d7d50676e6d0ea065f445d8a5539d')
    expect_equal(digest(round(as.numeric(sum(dist_matrix[2, ])))), 'e8c6c490beabf8e7ab89dcd59a75f389')
    expect_equal(digest(round(as.numeric(sum(dist_matrix[5, ])))), 'a86d0670df7fb4f1da7b38943f5ee4e7')
    expect_equal(digest(round(as.numeric(sum(dist_matrix[6, ])))), '4a5d7d50676e6d0ea065f445d8a5539d')
    })
print("Success!")
    }

test_2.3 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.3), '5b58e040ee35f3bcc6023fb7836c842e') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_2.4 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.4), '9c8cb5538e7778bf0b1bd53e45fb78c9') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_2.5 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.5), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_2.6 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.6), '9c8cb5538e7778bf0b1bd53e45fb78c9') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_2.7 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.7), '0590b0427c1b19a6eb612d19888aa52f') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }



test_3.0 <- function(){
test_that('Did not create an object named X_train', {
    expect_true(exists("X_train")) 
    })
test_that('X_train should be a data frame.', {
    expect_true('data.frame' %in% class(X_train))
    })
test_that('X_train does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(X_train), c(569, 2))
    })
test_that('X_train does not contain the column(s) Symmetry and/or Radius.', {
    expect_true('Symmetry' %in% colnames(X_train))
    expect_true('Radius' %in% colnames(X_train))
    })
test_that('Did not create an object named Y_train', {
    expect_true(exists("Y_train")) 
    })
test_that('Y_train should be a character.', {
    expect_true('character' %in% class(Y_train))
    })
test_that('Y_train is not the correct length.', {
    expect_equal(length(Y_train), 569)
    })
print("Success!")
    }

test_3.1 <- function(){
test_that('method should be knn', {
    expect_equal(model_knn$method, 'knn')
    })
test_that('k should be 7', {
    expect_equal(as.numeric(model_knn$results$k), 7)
    })
test_that('model_knn contains incorrect information.', {
    expect_equal(digest(round(as.numeric(sum(model_knn$trainingData$Symmetry)))), '908d1fd10b357ed0ceaaec823abf81bc')
    expect_equal(digest(round(as.numeric(sum(model_knn$trainingData$Radius)))), 'b01ec9609569aecde321d7dd2674f6e1')
    expect_equal(as.numeric(summary(model_knn$trainingData$.outcome)[1]), 357)
    })
print("Success!")
    }

test_3.2 <- function(){
test_that('Did not create an object named new_obs', {
    expect_true(exists("new_obs")) 
    })
test_that('new_obs should be a data frame.', {
    expect_true('data.frame' %in% class(new_obs))
    })
test_that('Did not create an object named predicted_knn_7', {
    expect_true(exists("predicted_knn_7")) 
    })
test_that('predicted_knn_7 should be a factor.', {
    expect_true('factor' %in% class(predicted_knn_7))
    })
test_that('predicted_knn_7 prediction is incorrect.', {
    expect_equal(digest(as.character(predicted_knn_7)), '5f0922939c45ef1054f852e83f91c660')
    })
print("Success!")
    }

test_3.3 <- function(){
test_that('Did not create an object named X_train_3', {
    expect_true(exists("X_train_3")) 
    })
test_that('X_train_3 should be a data frame.', {
    expect_true('data.frame' %in% class(X_train_3))
    })
test_that('X_train_3 does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(X_train_3), c(569, 3))
    })
test_that('Did not create an object named Y_train_3', {
    expect_true(exists("Y_train_3")) 
    })
test_that('Y_train_3 should be a character.', {
    expect_true('character' %in% class(Y_train_3))
    })
test_that('Y_train_3 is not the correct length.', {
    expect_equal(length(Y_train_3), 569)
    })
test_that('Did not create an object named new_obs_3', {
    expect_true(exists("new_obs_3")) 
    })
test_that('new_obs_3 should be a data frame.', {
    expect_true('data.frame' %in% class(new_obs_3))
    })
test_that('Did not create an object named predicted_3_knn_7', {
    expect_true(exists("predicted_3_knn_7")) 
    })
test_that('predicted_3_knn_7 should be a factor.', {
    expect_true('factor' %in% class(predicted_3_knn_7))
    })
test_that('predicted_3_knn_7 prediction is incorrect.', {
    expect_equal(digest(as.character(predicted_3_knn_7)), '5f0922939c45ef1054f852e83f91c660')
    })
print("Success!")
    }

test_3.4 <- function(){
test_that('Did not create an object named X_train_all', {
    expect_true(exists("X_train_all")) 
    })
test_that('X_train_all should be a data frame.', {
    expect_true('data.frame' %in% class(X_train_all))
    })
test_that('X_train_all does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(X_train_all), c(569, 10))
    })
test_that('Did not create an object named Y_train_all', {
    expect_true(exists("Y_train_all")) 
    })
test_that('Y_train_all should be a character.', {
    expect_true('character' %in% class(Y_train_all))
    })
test_that('Y_train_all is not the correct length.', {
    expect_equal(length(Y_train_all), 569)
    })
test_that('Did not create an object named new_obs_all', {
    expect_true(exists("new_obs_all")) 
    })
test_that('new_obs_all should be a data frame.', {
    expect_true('data.frame' %in% class(new_obs_all))
    })
test_that('Did not create an object named predicted_all_knn_7', {
    expect_true(exists("predicted_all_knn_7")) 
    })
test_that('predicted_all_knn_7 should be a factor.', {
    expect_true('factor' %in% class(predicted_all_knn_7))
    })
test_that('predicted_all_knn_7 prediction is incorrect.', {
    expect_equal(digest(as.character(predicted_all_knn_7)), '3a5505c06543876fe45598b5e5e5195d')
    })
print("Success!")
    }

test_4.0 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(as.character(answer4.0)), '75f1160e72554f4270c809f041c7a776')
})
print("Success!")
    }

test_4.1 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(as.character(answer4.1)), '475bf9280aab63a82af60791302736f6')
})
print("Success!")
    }