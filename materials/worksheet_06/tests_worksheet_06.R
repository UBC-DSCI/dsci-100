# # +
library(testthat)
library(digest)
library(rlang)

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
    xint1 = xint + 1L
    if (abs(xint - x) < abs(xint1 - x)){
        return(xint)
    }
    else {
        return(xint1)
    }
}
# -

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
    expect_equal(digest(int_round(sum(cancer$Area), 2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(colnames(cancer), c("ID", "Class", "Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity", "Concave_points", "Symmetry", "Fractal_dimension"))
})
test_that('read.csv() instead of read_csv() function is used.', {
    expect_true(class(cancer$Class) == 'character')
})
print("Success!")
    }

test_1.1 <- function(){
    if (digest(answer1.1) == '05ca18b596514af73f6880309a21b5dd'){
        print("Look at the values in the Area column - are they categorical? Remember, classification problems involve predicting class labels for categorical data.")
    }
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
test_that('Points should be coloured by Class.', {
    expect_true("Class" == rlang::get_expr(properties$colour))
    })
test_that('cancer_plot should be a scatter plot.', {
    expect_true("GeomPoint" %in% class(cancer_plot$layers[[1]]$geom))
    })
test_that('cancer_plot should map Class to colour.', {
    expect_true(digest(rlang::get_expr(properties$colour)) %in% c('a4abb3d43fde633563dd1f5c3ea31f31', 'f9e884084b84794d762a535f3facec85'))
    })
test_that('axis labels do not state that the data is standardized (which it is!)', {
    expect_true(labels$x != "Symmetry")
    expect_true(labels$y != "Radius")
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
    expect_equal(digest(int_round(xa, 2)), 'c0048c4f8677b795155d8aa41e26a54d')
    })
test_that('ya is incorrect.', {
    expect_equal(digest(int_round(ya, 2)), 'a6e8462a7cace5673e544d1e8d238b52')
    })
test_that('xb is incorrect.', {
    expect_equal(digest(int_round(xb, 2)), '10aeddd8594c6ce210c731b8b94af435')
    })
test_that('yb is incorrect.', {
    expect_equal(digest(int_round(yb, 2)), '48139aad2994737e7e801156a24281ed')
    })
print("Success!")
    }

test_1.5 <- function(){
test_that('answer1.5 is incorrect', {    
    expect_equal(digest(int_round(answer1.5, 2)), 'a95ceee8390cb47bb05410a8d23c76cf') # we hid the answer to the test here so you can't see it, but we can still run the test
    })
print("Success!")
    }

test_1.6 <- function(){
test_that('Did not create an object named zb', {
    expect_true(exists('zb'))
})
test_that('zb is incorrect.', {
    expect_equal(digest(int_round(zb,2)), 'b78a46ebc0bb9a4cc7f4f4b962f0b2ef')
    })
test_that('Did not create an object named za', {
    expect_true(exists('za'))
})
test_that('za is incorrect.', {
    expect_equal(digest(int_round(za,2)), 'b35d8adab2b7c839e5a8e2861080b03e')
    })
print("Success!")
    }

test_1.7 <- function(){
test_that('answer1.7 is incorrect', {    
    expect_equal(digest(int_round(answer1.7, 2)), 'c7fd80062a02f15d212704a20fae75fb') # we hid the answer to the test here so you can't see it, but we can still run the test
    })
print("Success!")
    }

test_1.8 <- function(){
  test_that('point_a is incorrect.', {
    expect_equal(digest(int_round(sum((point_a)),2)), '44014eaa19f1aef8e92b1020c47d662b')
      })
test_that('point_b is incorrect.', {
    expect_equal(digest(int_round(sum((point_b)),2)), 'e064b40c9ca28b04b874bcd8bdefa41e')
    })
print("Success!")
    }

test_1.09 <- function(){
test_that('dif_square is incorrect', {    
    expect_equal(digest(int_round(sum(dif_square),2)), 'e276884e43714ac361db1a1998bb6bc9') # we hid the answer to the test here so you can't see it, but we can still run the test
})
print("Success!")
    }


test_1.09.1 <- function(){
test_that('dif_sum is incorrect', {    
    expect_equal(digest(int_round(dif_sum,2)), 'e276884e43714ac361db1a1998bb6bc9') # we hid the answer to the test here so you can't see it, but we can still run the test
})
print("Success!")
}

test_1.09.2 <- function(){
test_that('root_dif_sum is incorrect', {    
    expect_equal(digest(int_round(root_dif_sum,2)), 'c7fd80062a02f15d212704a20fae75fb') # we hid the answer to the test here so you can't see it, but we can still run the test
})
print("Success!")
}

test_1.09.3 <- function(){
test_that('dist_cancer_two_rows is incorrect', {    
    expect_equal(digest(int_round(dist_cancer_two_rows,2)), 'c7fd80062a02f15d212704a20fae75fb') # we hid the answer to the test here so you can't see it, but we can still run the test

})
print("Success!")
}


test_1.09.4 <- function(){
test_that('Solution is incorrect', {    
    expect_equal(digest(answer1.09.4), '05ca18b596514af73f6880309a21b5dd') # we hid the answer to the test here so you can't see it, but we can still run the test

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
    expect_equal(digest(int_round(sum(small_sample_plot$data$Symmetry),2)), '727b6cd45f0340de38d1cfe8403adb3e')
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
test_that('axis labels do not state that the data is standardized (which it is!)', {
    expect_true(labels$x != "Symmetry")
    expect_true(labels$y != "Radius")
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
test_that('The last row of the Class column should be "unknown".', {
    expect_equal((newData$Class[6]), 'unknown')
    })
test_that('newData does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(newData), c(6, 3))
    })
test_that('small_sample does not contain the correct data.', {
    expect_equal(digest(int_round(sum(newData$Radius),2)), '740dbeffda6d0ffb1b86f797df4c2a25')
    expect_equal(digest(int_round(sum(newData$Symmetry),2)), 'a14e0862232f39bc203a2c5021371b54')
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
    expect_equal(digest(int_round(sum(dist_matrix[1, ]),2)), '8435efbd9cf83356ac4a26c6889c8fa5')
    expect_equal(digest(int_round(sum(dist_matrix[2, ]),2)), 'abc3f5458b96456d63865f0922593548')
    expect_equal(digest(int_round(sum(dist_matrix[5, ]),2)), 'c3ad708acb2b90a9e40e48f729083e69')
    expect_equal(digest(int_round(sum(dist_matrix[6, ]),2)), 'f0f12367a5beee1f65d2633294474dc9')
    })
print("Success!")
    }

test_2.3 <- function(){
    if (typeof(answer2.3) == 'double') {
        print("Remember to surround your answer with quotes!")
    }
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.3), 'ee340e888492be0703f2bcc9abfb390c') # we hid the answer to the test here so you can't see it, but we can still run the test
    })
print("Success!")
    }

test_2.4 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.4), '891e8a631267b478c03e25594808709d') # we hid the answer to the test here so you can't see it, but we can still run the test
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
    expect_equal(digest(answer2.7), '863dfc36ab2bfe97404cc8fc074a5241') # we hid the answer to the test here so you can't see it, but we can still run the test

})
print("Success!")
    }

test_3.1 <- function(){
test_that('Did not create an object named knn_spec', {
    expect_true(exists("knn_spec"))
    })
test_that('k should be 7', {
    expect_equal(as.numeric(get_expr(knn_spec$args$neighbors)), 7)
    })
test_that('weight_func is incorrect', {
    expect_equal(digest(as.character(get_expr(knn_spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    })
test_that('set_engine is incorrect', {
    expect_equal(digest(as.character(knn_spec$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
    })
test_that('mode is incorrect', {
    expect_equal(digest(as.character(knn_spec$mode)), 'f361ba6f6b32d068e56f61f53d35e26a')
    })
print("Success!")
    }

test_3.2 <- function(){
    test_that('Did not create an object named knn_fit', {
        expect_true(exists("knn_fit")) 
    })
    test_that('knn_fit should be a fit model.', {
        expect_true('model_fit' %in% class(knn_fit))
    })
    test_that('knn_fit does not include cancer_train dataset', {
        expect_equal(digest(as.character(knn_fit$fit$data$Class)), '93ecaae439b9f4e8e4297d3a851929f9')
        expect_equal(digest(int_round(sum(knn_fit$fit$data$Symmetry),2)), '1473d70e5646a26de3c52aa1abd85b1f')
        expect_equal(digest(int_round(sum(knn_fit$fit$data$Radius),2)), '1473d70e5646a26de3c52aa1abd85b1f') 
    })
    test_that('knn_fit does not contain knn_spec', {
        expect_equal(digest(int_round(get_expr(knn_fit$spec$args$neighbors),2)), '51465273097370367115dfe0228831f3')
        expect_equal(digest(as.character(get_expr(knn_fit$spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
        expect_equal(digest(knn_fit$spec$mode), 'f361ba6f6b32d068e56f61f53d35e26a')
    })
    test_that('knn_fit does not use the correct columns and/or the correct model formula', {
        expect_setequal(c('Class', 'Radius', 'Symmetry'), row.names(attributes(knn_fit$fit$terms)$factors))
    }) 
print("Success!")
    }

test_3.3 <- function(){
test_that('Did not create an object named new_obs',{
    expect_true(exists("new_obs"))
    })
test_that('new_obs is not a tibble', {
    expect_true('data.frame' %in% class(new_obs))
    })
test_that('Wrong values for Symmetry and Radius', {
    expect_equal(as.numeric(new_obs$Symmetry), 1)
    expect_equal(as.numeric(new_obs$Radius), 0)
})
test_that('Did not create an object named class_prediction',{
    expect_true(exists("class_prediction"))
    })
test_that('Wrong class prediction', {
    expect_equal(digest(as.character(class_prediction$.pred_class)), '5f0922939c45ef1054f852e83f91c660')
    })
print("Success!")
    }

test_3.4 <- function(){
test_that('k should be 7', {
    expect_equal(int_round(get_expr(knn_spec$args$neighbors),0), 7)
    })
test_that('weight_func is incorrect', {
    expect_equal(digest(as.character(get_expr(knn_spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    })
test_that('set_engine is incorrect', {
    expect_equal(digest(as.character(knn_spec$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
    })
test_that('mode is incorrect', {
    expect_equal(digest(as.character(knn_spec$mode)), 'f361ba6f6b32d068e56f61f53d35e26a')
    })
test_that('Did not create an object named knn_fit_2', {
    expect_true(exists("knn_fit_2")) 
    })
test_that('knn_fit_2 should be a fit model.', {
    expect_true('model_fit' %in% class(knn_fit_2))
    })
test_that('knn_fit_2 does not use the correct columns and/or the correct model formula', {
    expect_setequal(c('Class', 'Radius', 'Symmetry', 'Concavity'), row.names(attributes(knn_fit_2$fit$terms)$factors))
    })
test_that('knn_fit_2 does not include knn_train_2 dataset', {
    expect_equal(digest(as.character(knn_fit_2$fit$data$Class)), '93ecaae439b9f4e8e4297d3a851929f9')
    expect_equal(digest(int_round(sum(knn_fit_2$fit$data$Symmetry),2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(digest(int_round(sum(knn_fit_2$fit$data$Radius),2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(digest(int_round(sum(knn_fit_2$fit$data$Concavity),2)), '1473d70e5646a26de3c52aa1abd85b1f')
    })
test_that('knn_fit_2 does not contain knn_spec_2', {
    expect_equal(digest(as.numeric(get_expr(knn_fit_2$spec$args$neighbors))), '90a7653d717dc1553ee564aa27b749b9')
    expect_equal(digest(as.character(get_expr(knn_fit_2$spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    expect_equal(digest(knn_fit_2$spec$mode), 'f361ba6f6b32d068e56f61f53d35e26a')
    }) 
test_that('Did not create an object named new_obs_2',{
    expect_true(exists("new_obs_2"))
    })
test_that('new_obs_2 is not a tibble', {
    expect_true('data.frame' %in% class(new_obs_2))
    })
test_that('Wrong values for Symmetry, Radius, and Concavity', {
    expect_equal(int_round(new_obs_2$Symmetry, 0), 1)
    expect_equal(int_round(new_obs_2$Radius, 0), 0)
    expect_equal(int_round(new_obs_2$Concavity, 0), 1)
})
test_that('Did not create an object named class_prediction_2',{
    expect_true(exists("class_prediction_2"))
    })
test_that('Wrong class prediction', {
    expect_equal(digest(as.character(class_prediction_2$.pred_class)), '5f0922939c45ef1054f852e83f91c660')
    }) 
print("Success!")
}

test_3.5 <- function(){
test_that('Did not create a object named knn_recipe', {
    expect_true(exists("knn_recipe"))
    })
test_that('knn_recipe is not a recipe object', {
    expect_equal(digest(class(knn_recipe)), '4b3ed1334bff94d43e32a36a1f16a2f2')
    })
test_that('knn_recipe does not remove ID', {    
    expect_false("ID" %in% (knn_recipe %>% prep() %>% bake(cancer) %>% colnames()))
})
test_that('cancer does not contain the correct data.', {
    expect_equal(dim(bake(prep(knn_recipe), cancer)), c(569,11))
})
print("Success!")
}

test_3.6 <- function(){
    test_that('Did not create an object named knn_workflow', {
    expect_true(exists("knn_workflow"))
    })
    test_that('knn_workflow is not a workflow', {
    expect_true('workflow' %in% class(knn_workflow))
    })
    test_that('knn_workflow does not contain the right model specification', {
    expect_equal(int_round(get_expr(knn_workflow$fit$actions$model$spec$args$neighbors),0), 7)
    })
    test_that('Did not add knn_recipe', {
    expect_true('recipe' %in%  class(knn_workflow$pre$actions$recipe$recipe))
    })
    test_that('knn_recipe does not contain the cancer dataset', {
    expect_equal(digest(int_round(sum(knn_workflow$pre$actions$recipe$recipe$template$Radius),2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(digest(int_round(sum(knn_workflow$pre$actions$recipe$recipe$template$Area),2)), '1473d70e5646a26de3c52aa1abd85b1f')    
    })
print("Success!")
}

test_3.7 <- function(){
    test_that('Did not create an object named class_prediction_all',{
    expect_true(exists("class_prediction_all"))
    })
test_that('Wrong class prediction', {
    expect_equal(digest(as.character(class_prediction_all$.pred_class)), '3a5505c06543876fe45598b5e5e5195d')
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
