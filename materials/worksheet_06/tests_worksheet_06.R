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
    expect_equal(digest(as.numeric(sum(cancer$Area))), 'a2c1855f3fa92423aa169c350fc95232')
    expect_equal(colnames(cancer), c("ID", "Class", "Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity", "Concave_points", "Symmetry", "Fractal_dimension"))
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
    expect_that("Symmetry" == rlang::get_expr(properties$x), is_true())
    })
test_that('Radius should be on the y-axis.', {
    expect_that("Radius" == rlang::get_expr(properties$y) , is_true())
    })
test_that('cancer_plot should be a scatter plot.', {
    expect_that("GeomPoint" %in% class(cancer_plot$layers[[1]]$geom) , is_true())
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
    expect_equal(digest(as.numeric(xa)), '218983ef51880f784c62ff2aedc196f3')
    })
test_that('ya is incorrect.', {
    expect_equal(digest(as.numeric(ya)), 'a1914c10445a398934c0e0015b9b18ae')
    })
test_that('xb is incorrect.', {
    expect_equal(digest(as.numeric(xb)), '5b34d8796880663f75ea423ccb4ea8cd')
    })
test_that('yb is incorrect.', {
    expect_equal(digest(as.numeric(yb)), '4490c7a115f39cede8cd353713230e95')
    })
print("Success!")
    }

test_1.5 <- function(){
test_that('answer1.5 is incorrect', {    
    expect_equal(digest(as.numeric(answer1.5)), 'ab39ff487bddaa92a62eadbbe3e46da6') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.6 <- function(){
test_that('zb is incorrect.', {
    expect_equal(digest(as.numeric(zb)), 'b62bcabaf783e3e2d0745ca4a41219da')
    })
test_that('za is incorrect.', {
    expect_equal(digest(as.numeric(za)), '8f22ef4a815b2e1bd4f7ec511bbc30f2')
    })
print("Success!")
    }

test_1.7 <- function(){
test_that('answer1.7 is incorrect', {    
    expect_equal(digest(as.numeric(answer1.7)), '97c5e6129bc96a23ed7298d78bf7f8b2') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }



test_1.8 <- function(){
  test_that('point_a is incorrect.', {
    expect_equal(digest(as.numeric(sum(point_a))), '309d3b37c196b24341299aabdac15644')
    })
test_that('point_b is incorrect.', {
    expect_equal(digest(as.numeric(sum(point_b))), '00bb41bc0f538b06f627ffbd9874a6a8')
    })
print("Success!")
    }

test_1.9 <- function(){
test_that('difference is incorrect', {    
    expect_equal(digest(as.numeric(sum(difference))), 'ef1fc2c1e06df149b42dcfb47596319f') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.10 <- function(){
test_that('dif_square is incorrect', {    
    expect_equal(digest(as.numeric(sum(dif_square))), '0299530505a02b47c2a30af0ecd6026b') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_1.10.1 <- function(){
test_that('dif_sum is incorrect', {    
    expect_equal(digest(as.numeric(dif_sum)), '0299530505a02b47c2a30af0ecd6026b') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
}

test_1.10.2 <- function(){
test_that('root_dif_sum is incorrect', {    
    expect_equal(digest(as.numeric(root_dif_sum)), '97c5e6129bc96a23ed7298d78bf7f8b2') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
}

test_1.10.3 <- function(){
test_that('dist_cancer_two_rows is incorrect', {    
    expect_equal(digest(as.numeric(dist_cancer_two_rows)), '97c5e6129bc96a23ed7298d78bf7f8b2') # we hid the answer to the test here so you can't see it, but we can still run the test
    
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
test_that('small_sample does not contain the correct data.', {
    expect_equal(digest(as.numeric(sum(small_sample$Symmetry))), '9335520ab7f7f606573cad5c82a24542')
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
    expect_that("Symmetry" == rlang::get_expr(properties$x), is_true())
    })
test_that('Radius should be on the y-axis.', {
    expect_that("Radius" == rlang::get_expr(properties$y) , is_true())
    })
test_that('small_sample_plot should be a scatter plot.', {
    expect_that("GeomPoint" %in% c(class(small_sample_plot$layers[[1]]$geom)) , is_true())
    })
test_that('small_sample_plot should map Benign / Malignant to colour.', {
    expect_that("Class" == rlang::get_expr(properties$colour) , is_true())
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
    expect_equal(newData$Class[6], 'unknown')
    })
test_that('newData does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(newData), c(6, 3))
    })
test_that('small_sample does not contain the correct data.', {
    expect_equal(digest(sum(as.numeric(newData$Radius))), '291661ebdc869b7f43db4135b6dff842')
    expect_equal(digest(sum(as.numeric(newData$Symmetry))), '9335520ab7f7f606573cad5c82a24542')
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
    expect_equal(digest(as.numeric(sum(dist_matrix[1, ]))), digest(sum(dist_matrix[, 1])))
    expect_equal(digest(as.numeric(sum(dist_matrix[2, ]))), digest(sum(dist_matrix[, 2])))
    expect_equal(digest(as.numeric(sum(dist_matrix[5, ]))), digest(sum(dist_matrix[, 5])))
    expect_equal(digest(as.numeric(sum(dist_matrix[6, ]))), digest(sum(dist_matrix[, 6])))
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
    expect_equal(as.character(model_knn$method), 'knn')
    })
test_that('k should be 7', {
    expect_equal(as.numeric(model_knn$results$k), 7)
    })
test_that('model_knn contains incorrect information.', {
    expect_equal(digest(as.numeric(sum(model_knn$trainingData$Symmetry))), '47d0e881a9a1b19e57f9c068c08765fa')
    expect_equal(digest(as.numeric(sum(model_knn$trainingData$Radius))), '5818709a65b4a5df9cb392b9cc66e32b')
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