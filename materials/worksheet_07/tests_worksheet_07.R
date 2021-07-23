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

test_0.1 <- function(){
test_that('Did not create an object named answer0.1', {
    expect_true(exists('answer0.1'))
    })
test_that('Solution is incorrect', {
    expect_equal(digest(answer0.1), '475bf9280aab63a82af60791302736f6')
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
    expect_equal(digest(int_round(sum(fruit_data$mass, na.rm = TRUE), 2)), '8c7433f4d278ef1e1e0f8d0ccb217614') # we hid the answer to the test here so you can't see it, but we can still run the test  
})
print("Success!")
    }

# # +
test_1.1 <- function(){
test_that('Did not create an object called answer1.1', {
    expect_true(exists('answer1.1'))
})
test_that('Solution is incorrect', {
    expect_equal(digest(answer1.1), '75f1160e72554f4270c809f041c7a776') # we hid the answer to the test here so you can't see it, but we can still run the test

})
print("Success!")
    }
# -

test_1.2 <- function(){
test_that('Did not create an object named fruit_dist_2', {
    expect_true(exists("fruit_dist_2")) 
})
test_that('fruit_dist_2 is incorrect.', {
    expect_equal(digest(int_round(fruit_dist_2, 3)), 'a29a5d18050c6ce0aa2dc501684e1375')
    })
print("Success!")
    }

test_1.3 <- function(){
test_that('Did not create an object named fruit_dist_44', {
    expect_true(exists("fruit_dist_44")) 
})
test_that('fruit_dist_44 is incorrect.', {
    expect_equal(digest(int_round(fruit_dist_44, 2)), 'ea07cf8b74030ff04b56ac69dd094adc')
    })
print("Success!")
    }


test_1.4 <- function(){
test_that('Did not create an object named answer1.4', {
    expect_true(exists('answer1.4'))
})
test_that('Solution is incorrect', {
    expect_equal(digest(answer1.4), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
})
print("Success!")
    }

test_1.5 <- function(){
test_that('Did not create an object named fruit_data_scaled', {
    expect_true(exists("fruit_data_scaled")) 
})
test_that('fruit_data_scaled does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(fruit_data_scaled), c(59, 11))
})
test_that('The fruit_name column in fruit_data_scaled should be of class factor.', {
    expect_true(is.factor(fruit_data_scaled$fruit_name))
})
test_that('Columns in fruit_data_scaled contain incorrect values.', {
    expect_equal(digest(int_round(sum(fruit_data_scaled$mass, na.rm = TRUE), 2)), '8c7433f4d278ef1e1e0f8d0ccb217614') # we hid the answer to the test here so you can't see it, but we can still run the test  
})
test_that('The mass, height, color score, and width columns in fruit_data_Scaled should be scaled.', {
    expect_equal(digest(int_round(sum(fruit_data_scaled$scaled_mass), 2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(digest(int_round(sum(fruit_data_scaled$scaled_width), 2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(digest(int_round(sum(fruit_data_scaled$scaled_height), 2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(digest(int_round(sum(fruit_data_scaled$scaled_color_score), 2)), '1473d70e5646a26de3c52aa1abd85b1f')
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
    expect_equal(digest(int_round(distance_2, 2)), '192b298ed4661ab6d9a4a193b2e60b49')
})
test_that('distance_2 is incorrect.', {
    expect_equal(digest(int_round(distance_44, 2)), '78f799aab6957dffdfd2bfb504f8cab5')
})
print("Success!")
    }

test_2.0 <- function(){
test_that('Did not create an object named fruit_train', {
    expect_true(exists("fruit_train")) 
})
test_that('Did not create an object named fruit_test', {
    expect_true(exists("fruit_test")) 
})    
test_that('fruit_train does not contain the correct number of rows and/or columns', {
    expect_equal(dim(fruit_train), c(46, 7))
})
test_that('fruit_test does not contain the correct number of rows and/or columns',{
    expect_equal(dim(fruit_test), c(13, 7))
})    
test_that('fruit_train contains the wrong data', {
    expect_equal(digest(int_round(sum(fruit_train$mass), 2)), 'a42fff2d173ac77a5198b1e8422cb9ba')
})
test_that('fruit_test contains the wrong data', {
    expect_equal(digest(int_round(sum(fruit_test$mass), 2)), 'fcb11cb4ce2aa88708f4f5895d59abbe')
})    
print("Success!")
    }


test_2.1 <- function(){
test_that('Did not create an object named fruit_recipe', {
    expect_true(exists("fruit_recipe")) 
    })
test_that('fruit_recipe should be a recipe.', {
    expect_true('recipe' %in% class(fruit_recipe))
    })
test_that('fruit_recipe contains the wrong columns', {
    expect_true('color_score' %in% colnames(fruit_recipe$template))
    expect_true('mass' %in% colnames(fruit_recipe$template))
    expect_true('fruit_name' %in% colnames(fruit_recipe$template))
    })
test_that('fruit_recipe contains the wrong data', {
    expect_equal(digest(int_round(sum(fruit_recipe$template$mass), 2)), 'a42fff2d173ac77a5198b1e8422cb9ba')
    expect_equal(digest(int_round(sum(fruit_recipe$template$color_score), 2)), '656cbb68c33ed8c769ed3fb3a423f886')
    })    
test_that('all_predictors() is not scaled and centered', {
    expect_equal(digest(as.character(get_expr(fruit_recipe$steps[[1]]$terms))), 'f34b27deb5a8023de51b602e9aacf535')
    expect_equal(digest(as.character(get_expr(fruit_recipe$steps[[2]]$terms))), 'f34b27deb5a8023de51b602e9aacf535')
})
test_that('fruit_name was not placed before predictors', {
    expect_equal(digest(as.character(fruit_recipe$var_info$variable[3])), '1298acdeb848b96767603d30382d6aff')
})
print("Success!")
    }

test_2.2 <- function(){
test_that('Did not create an object named knn_spec', {
    expect_true(exists("knn_spec"))
    })
test_that('knn_spec should be a model specification', {
    expect_true("model_spec" %in% class(knn_spec))
    })
test_that('k is not 3', {
    expect_equal(int_round(get_expr(knn_spec$args$neighbors), 0), 3)
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
test_that('Did not create an object named fruit_fit', {
    expect_true(exists("fruit_fit")) 
    })
test_that('fruit_fit should be a workflow.', {
    expect_true('workflow' %in% class(fruit_fit))
    })
test_that('fruit_fit does not contain scaled data', {
    expect_equal(digest(int_round(sum(fruit_fit$pre$mold$predictors$mass), 2)), '1473d70e5646a26de3c52aa1abd85b1f')
    expect_equal(digest(int_round(sum(fruit_fit$pre$mold$predictors$color_score), 2)), '1473d70e5646a26de3c52aa1abd85b1f')
})
print("Success!")    
    }

test_2.3 <- function(){
test_that('Did not create an object named fruit_test_predictions', {
    expect_true(exists("fruit_test_predictions")) 
    })
test_that('fruit_test_predictions should be a tibble.', {
    expect_true('tbl' %in% class(fruit_test_predictions))
    })
test_that('fruit_test_predictions should contain the original data and the new prediction column', {
    expect_equal(dim(fruit_test_predictions), c(13, 8))
    expect_true('.pred_class' %in% colnames(fruit_test_predictions))
    })    
print("Success!")
    }

test_2.4 <- function(){
test_that('Did not create an object named fruit_prediction_accuracy', {
    expect_true(exists("fruit_prediction_accuracy")) 
    })
test_that('fruit_prediction_accuracy should be a tibble', {
    expect_true('tbl' %in% class(fruit_prediction_accuracy))
    })
test_that('estimates are incorrect', {
    expect_equal(digest(int_round(sum(fruit_prediction_accuracy$.estimate), 2)), '44865f1c212fb27ca7ab5b7154dcf398')
    })
test_that('the estimator should be a multiclass classification', {
    expect_true('multiclass' %in% fruit_prediction_accuracy$.estimator)
})
print("Success!")
    }

test_2.5 <- function(){
test_that('Did not create an object named fruit_mat', {
    expect_true(exists("fruit_mat")) 
    })
test_that('fruit_mat is not a confusion matrix', {
    expect_true('conf_mat' %in% class(fruit_mat))
    })
test_that('Number of observations is incorrect', {
    expect_equal(digest(int_round(sum(as.tibble(fruit_mat$table)[3]), 2)), '306a937dfa0335e74514e4c6044755f6')
    })
print("Success!")
    }


test_2.6 <- function(){
test_that('Did not create an object named answer2.6', {
    expect_true(exists("answer2.6"))
})
test_that('Answer is incorrect', {
    expect_equal(digest(answer2.6), 'c1f86f7430df7ddb256980ea6a3b57a4')
})
print("Success!")
    }

test_3.1 <- function(){
test_that('Did not create an object named fruit_vfold', {
    expect_true(exists("fruit_vfold")) 
})
test_that('fruit_vfold is not a cross validation object',{
    expect_true('vfold_cv' %in% class(fruit_vfold))
})
test_that('fruit_vfold does not contain 5 folds', {
    expect_equal(int_round(length(fruit_vfold$id), 0), 5)
})
test_that('fruit_vfold contains the incorrect data', {
    expect_equal(dim(fruit_vfold), c(5, 2))
})
test_that('fruit_vfold does not use the training data', {
    expect_equal(digest(int_round(sum(fruit_vfold$splits[[1]]$data$color_score), 2)), '656cbb68c33ed8c769ed3fb3a423f886')
    expect_equal(digest(int_round(sum(fruit_vfold$splits[[1]]$data$mass), 2)), 'a42fff2d173ac77a5198b1e8422cb9ba')
})
test_that('strata argument is not fruit_name', {
    expect_equal(digest(int_round(sum(fruit_vfold$splits[[1]]$in_id), 2)), 'df9b1bae6656d96dfbe896782bd9de05')
    expect_equal(digest(int_round(sum(fruit_vfold$splits[[2]]$in_id), 2)), 'd01f1e59ae3a6b2db6831e601606b1c0')
    expect_equal(digest(int_round(sum(fruit_vfold$splits[[3]]$in_id), 2)), '71321611fabe5aee0df74bb96fe3a545')
    expect_equal(digest(int_round(sum(fruit_vfold$splits[[4]]$in_id), 2)), 'd01f1e59ae3a6b2db6831e601606b1c0')
    expect_equal(digest(int_round(sum(fruit_vfold$splits[[5]]$in_id), 2)), '28db4e404a1c7e394adb8f9a21711424')
})
print("Success!")
    }

test_3.2 <- function(){
test_that('Did not create an object named fruit_resample_fit', {
    expect_true(exists("fruit_resample_fit")) 
})
test_that('fruit_resample_fit is not a resample_result', {
    expect_true('resample_results' %in% class(fruit_resample_fit))
})
test_that('fruit_resample_fit contains the incorrect data', {
    expect_equal(dim(fruit_resample_fit), c(5, 4))
})
test_that('number of splits is not 5' ,{
    expect_equal(int_round(length(fruit_resample_fit$splits), 0), 5)
})
test_that('fruit_vfold should contain 5 folds', {
    expect_equal(int_round(length(fruit_vfold$id), 0), 5)
})
test_that('fruit_vfold contains the incorrect data', {
    expect_equal(dim(fruit_vfold), c(5, 2))
})
print("Success!")
    }

test_3.3 <- function(){
test_that('Did not create an object named fruit_metrics', {    
    expect_true(exists("fruit_metrics")) 
})
test_that('fruit_metrics contains the wrong data', {
    expect_equal(dim(fruit_metrics), c(2, 5))
    expect_true('mean' %in% colnames(fruit_metrics))
    expect_true('std_err' %in% colnames(fruit_metrics))
    expect_equal(digest(int_round(sum(fruit_metrics$mean), 2)), '3bb12916e7f6fda4645dd4ecaedb76b9')
    expect_equal(digest(int_round(sum(fruit_metrics$std_err), 2)), '8eaca7c9b35d05ab15c9125bc92372fa')
    expect_equal(digest(int_round(sum(fruit_metrics$n), 2)), 'b6a6227038bf9be67533a45a6511cc7e')
})
print("Success!")
    }

test_4.0 <- function(){
test_that('Did not create an object named knn_tune', {
    expect_true(exists("knn_tune"))
    })
test_that('knn_tune should be a model specification', {
    expect_true("model_spec" %in% class(knn_tune))
    })
test_that('k is not set to tune', {
    expect_equal(as.character(get_expr(knn_tune$args$neighbors)), 'tune')
    })
test_that('weight_func is incorrect', {
    expect_equal(digest(as.character(get_expr(knn_tune$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    })
test_that('set_engine is incorrect', {
    expect_equal(digest(as.character(knn_tune$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
    })
test_that('mode is incorrect', {
    expect_equal(digest(as.character(knn_tune$mode)), 'f361ba6f6b32d068e56f61f53d35e26a')
    })
print("Success!")
    }

test_4.1 <- function(){
test_that('Did not create an object called knn_results',{
    expect_true(exists("knn_results"))
})
test_that('knn_results should be a tibble', {
    expect_true('tbl' %in% class(knn_results))
})
test_that('knn_results does not contain the correct data',{
    expect_equal(dim(knn_results), c(20, 7))
    expect_equal(digest(int_round(sum(knn_results$neighbors), 2)), 'bc0bb1b780c5a2b3fbe18f1017288655')
    expect_equal(digest(int_round(sum(knn_results$mean), 2)), '2fbed5c22f5fcb638e9cad6f0d588e47')
    expect_equal(digest(int_round(sum(knn_results$std_err), 2)), '6c3a3556917f12517be89f353d7b93ff')
})
test_that('grid is not set to 10', {
    expect_equal(int_round(length(unique(knn_results$.config)), 0), 10)
})
print("Success!")
    }


test_4.2 <- function(){
test_that('Did not create an object called accuracies', {
    expect_true(exists("accuracies"))
})
test_that('accuracies .metric column should only contain accuracy', {
    expect_true(unique(accuracies$.metric) == "accuracy")
})
    properties <- c(accuracy_versus_k$layers[[1]]$mapping, accuracy_versus_k$mapping)
    labels <- accuracy_versus_k$labels
test_that('Did not create a plot named accuracy_versus_k', {
    expect_true(exists("accuracy_versus_k")) 
})
test_that('neighbors should be on the x-axis.', {
    expect_true("neighbors" == rlang::get_expr(properties$x))
})
test_that('mean should be on the y-axis.', {
    expect_true("mean" == rlang::get_expr(properties$y))
})
test_that('accuracy_versus_k should be a scatter/line plot.', {
    expect_true("GeomPoint" %in% c(class(accuracy_versus_k$layers[[1]]$geom)))
    expect_true("GeomLine" %in% c(class(accuracy_versus_k$layers[[2]]$geom)))
})
print("Success!")
    }
