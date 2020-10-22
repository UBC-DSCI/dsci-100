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
    expect_equal(digest(int_round(sum(fruit_metrics$mean), 2)), '99496b18802c49d6571120c16308addc')
    expect_equal(digest(int_round(sum(fruit_metrics$std_err), 2)), '234a2a5581872457b9fe1187d1616b13')
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
    expect_equal(digest(int_round(sum(knn_results$mean), 2)), 'e948a7acb8065d6583060c83b763e871')
    expect_equal(digest(int_round(sum(knn_results$std_err), 2)), '2af87dfdb66a77e47ec325e40c275f91')
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
