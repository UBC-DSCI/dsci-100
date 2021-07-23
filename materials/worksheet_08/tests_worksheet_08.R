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

test_0.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.0), '3a5505c06543876fe45598b5e5e5195d')
    })
    print("Success!")
}

test_0.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.1), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_0.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(int_round(answer0.2, 2)), '6953b334169bd7ec7da1c1eda5aaf6a5')
    })
    print("Success!")
}

test_0.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.3), '75f1160e72554f4270c809f041c7a776')
    })
    print("Success!")
}

test_1.0 <- function(){
    test_that('Did not create an object named marathon', {
        expect_true(exists("marathon")) 
    })
    test_that('marathon should be a tibble.', {
        expect_true('tbl' %in% class(marathon))
    })
    test_that('marathon does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon), c(929, 13))
    })
    test_that('The marathon tibble is missing columns.', {
        expect_true("time_hrs" %in% colnames(marathon))
        expect_true("max" %in% colnames(marathon))
    })
    print("Success!")
}

test_2.0 <- function(){
    properties <- c(answer2$mapping, answer2$layers[[1]]$mapping)
    labels <- answer2$labels
    test_that('Did not create a plot named answer2', {
        expect_true(exists("answer2")) 
    })
    test_that('marathon_50 does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon_50), c(50, 13))
    })
    test_that('answer2 should use information from marathon_50', {
        expect_equal(answer2$data, marathon_50) 
    })
    test_that('max should be on the x-axis.', {
        expect_true("max" %in% c(rlang::get_expr(properties$x),
                                 rlang::get_expr(properties$x)))
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_true("time_hrs" %in% c(rlang::get_expr(properties$y), 
                                      rlang::get_expr(properties$y)))
    })
    test_that('answer2 should be a scatter plot.', {
        expect_equal(digest(class(rlang::get_expr(answer2$layers[[1]]$geom))[1]), 
                 '911e5b9debfb523f25ad2ccc01a4b2dd')
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false((labels$y) == 'time_hrs')
        expect_false((labels$x) == 'max')
    })
print("Success!")  
}

test_3.0 <- function(){
    test_that('Did not create an object called answer3', {
        expect_true(exists('answer3'))
    })
    test_that('answer3 is incorrect', {
        expect_equal(digest(int_round(answer3, 1)), 'a266aa4a0aa711355be22e0f3b8d91af')
})
    print("Success!")
}

test_4.0 <- function(){
     test_that('Did not create an object called answer4', {
        expect_true(exists('answer4'))
    })
    test_that('answer4 is incorrect', {
        expect_equal(digest(int_round(answer4, 1)), '285d156b1b700fbb489df058fdb9e2ee')
    })
    print("Success!")
}

test_5.0 <- function(){
     test_that('Did not create an object called answer5', {
        expect_true(exists('answer5'))
    })
    test_that('Solution is incorrect', {
        expect_equal(digest(answer5), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_6.0 <- function(){
    test_that('Did not create an object named marathon_split', {
        expect_true(exists("marathon_split")) 
    })
    test_that('marathon_split should be rsplit (not a tibble)', {
        expect_true('rsplit' %in% class(marathon_split))
    })
    test_that('Did not create an object named marathon_training', {
        expect_true(exists('marathon_training'))
    })
    test_that('marathon_training does not contain 0.75 of the data.', {
        expect_equal(dim(marathon_training), c(698,13))
        expect_equal(digest(int_round(sum(marathon_training$age), 0)), '5109988e81575a4e65652fddb747a18f')
    })
    test_that('Did not create an object named marathon_testing', {
        expect_true(exists('marathon_testing'))
    })
    test_that('marathon_testing does not contain 0.25 of the data.', {
        expect_equal(dim(marathon_testing), c(231, 13))
        expect_equal(digest(int_round(sum(marathon_testing$age), 0)), '9ffe9b883d53974eaa9afb9bf0ec386b')
    })
    print("Success!")
}

test_7.0 <- function(){
    test_that('Did not create an object named marathon_spec', {
        expect_true(exists("marathon_spec")) 
    })
    test_that('neighbors argument is incorrect', {
        expect_equal(digest(as.character(get_expr(marathon_spec$args$neighbors))), '4b89cff22bb78b28a0a6b7fe28d371f6')
    })
    test_that('weight_func is incorrect', {
        expect_equal(digest(as.character(get_expr(marathon_spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    })
    test_that('set_engine is incorrect', {
        expect_equal(digest(as.character(marathon_spec$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
    })
    test_that('mode is incorrect', {
        expect_equal(digest(as.character(marathon_spec$mode)), 'b8bdd7015e0d1c6037512fd1396aef1a')
    })
    test_that('Did not create an object named marathon_recipe', {
        expect_true(exists("marathon_recipe"))
    })
    test_that('Data in marathon_recipe is not scaled and centered', {
        expect_equal(digest(int_round(sum(marathon_recipe$template$max), 0)), '94e91e5e6573ddfedb81802729c39543')
        expect_equal(digest(int_round(sum(marathon_recipe$template$time_hrs), 0)), '2213c3a0eb86305be22e0ca3b0a773c1')
    })
print("Success!")
}

test_7.1 <- function(){
    test_that('Did not create an object called marathon_vfold', {
        expect_true(exists("marathon_vfold"))
    })
    test_that('marathon_vfold does not contain 5 folds', {
        expect_equal(int_round(length(marathon_vfold$id), 0), 5)
    })
    test_that('marathon_vfold should be a cross-validation object', {
        expect_true('vfold_cv' %in% class(marathon_vfold))
    })
    test_that('Did not create an object called marathon_workflow', {
        expect_true(exists("marathon_workflow"))
    })
    test_that('marathon_workflow is not a workflow object', {
        expect_true('workflow' %in% class(marathon_workflow))
    })
    test_that('marathon_workflow does not contain the correct model specification', {
        expect_equal(digest(as.character(marathon_workflow$fit$actions$model$spec$args$neighbors)), 'b68c9f555cfd94fe903b741afcace6c1')
        expect_equal(digest(as.character(marathon_workflow$fit$actions$model$spec$mode)), 'b8bdd7015e0d1c6037512fd1396aef1a')
        expect_equal(digest(as.character(marathon_workflow$fit$actions$model$spec$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
        expect_true('nearest_neighbor' %in% class(marathon_workflow$fit$actions$model$spec))
    })
    test_that('marathon_workflow does not contain the correct recipe', {
        expect_true('recipe' %in% class(marathon_workflow$pre$actions$recipe$recipe))
        expect_equal(digest(int_round(sum(marathon_workflow$pre$actions$recipe$recipe$template$max), 0)), '94e91e5e6573ddfedb81802729c39543')
        expect_equal(digest(int_round(sum(marathon_workflow$pre$actions$recipe$recipe$template$time_hrs), 0)), '2213c3a0eb86305be22e0ca3b0a773c1')
    })
print("Success!")
    }


test_8.0 <- function(){
    test_that('Did not create an object named gridvals', {
        expect_true(exists('gridvals')) 
    })
    test_that('gridvals does not contain the correct data and column name', {
        expect_true('tbl' %in% class(gridvals))
        expect_true('neighbors' %in% colnames(gridvals))
        expect_equal(digest(int_round(sum(gridvals), 0)), '7f14b492534315a44244b698e5058e39') 
    })
    test_that('Did not create an object named marathon_results', {
        expect_true(exists('marathon_results'))
    })
    test_that('marathon_results is not a tibble', {
        expect_true('tbl' %in% class(marathon_results))
    })
    test_that('marathon_results does not contain the correct data', {
        expect_equal(dim(marathon_results), c(400, 7))
        expect_equal(digest(int_round(sum(marathon_results$neighbors), 0)), '07d8f52916463a9c6cd99797f0531d42')
        expect_equal(int_round(unique(marathon_results$n), 0), 5)
        expect_equal(digest(int_round(sum(marathon_results$mean), 0)), '3c9f3a7a14786e414122855e84509f9d')
        expect_equal(digest(int_round(sum(marathon_results$std_err), 0)), '7c7124efff5c7039a1b1e7cba65c5379')
    })
print("Success!")
}


test_8.1 <- function(){
    test_that('Did not create an object named marathon_min', {
        expect_true(exists('marathon_min'))
   })
    test_that('marathon_min is not a tibble', {
        expect_true('tbl' %in% class(marathon_min))
   })
    test_that('marathon_min does not contain the correct data', {
        expect_equal(dim(marathon_min), c(1, 7))
        expect_true('neighbors' %in% colnames(marathon_min))
        expect_true('.metric' %in% colnames(marathon_min))
        expect_true('.estimator' %in% colnames(marathon_min))
        expect_true('mean' %in% colnames(marathon_min))
        expect_true('n' %in% colnames(marathon_min))
        expect_true('std_err' %in% colnames(marathon_min))
        expect_true('.config' %in% colnames(marathon_min))
   })
    test_that('Best K value is incorrect', {
        expect_equal(digest(int_round(marathon_min$neighbors, 2)), '9241e88f7548d793a2482a33d623b99f')
   })
    test_that('Metric is incorrect', {
        expect_equal(digest(marathon_min$.metric), '91a8c46d46a2a25459eaabfa08f35967')
    })
    print("Success!")
}

test_8.2 <- function(){
    test_that('Did not create an object named k_min', {
        expect_true(exists('k_min'))
    })
    test_that('k_min is not correct', {
        expect_true('integer' %in% class(k_min))
        expect_equal(digest(int_round(k_min, 2)), '9241e88f7548d793a2482a33d623b99f')
    })
    test_that('Did not create an object named marathon_best_spec', {
        expect_true(exists('marathon_best_spec'))
    })
    test_that('marathon_best_spec has incorrect specifications', {
        expect_equal(digest(as.character(get_expr(marathon_best_spec$args$neighbors))), '0b942c90bc01f15b084d00fa29bf4cc0')
    })
    test_that('weight_func is incorrect', {
        expect_equal(digest(as.character(get_expr(marathon_best_spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    })
    test_that('set_engine is incorrect', {
        expect_equal(digest(as.character(marathon_best_spec$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
    })
    test_that('mode is incorrect', {
        expect_equal(digest(as.character(marathon_best_spec$mode)), 'b8bdd7015e0d1c6037512fd1396aef1a')
    })
    test_that('Did not create an object named marathon_best_fit', {
        expect_true(exists('marathon_best_fit'))
    })
     test_that('marathon_best_fit should be a workflow', {
        expect_true('workflow' %in% class(marathon_best_fit))
    })
    test_that('marathon_best_fit does not contain the correct model specification', {
        expect_equal(digest(get_expr(marathon_best_fit$fit$actions$model$spec$args$neighbors)), '7ad692ee809beafa13e6d291d0d5372f')
        expect_equal(digest(as.character(marathon_best_fit$fit$actions$model$spec$mode)), 'b8bdd7015e0d1c6037512fd1396aef1a')
        expect_equal(digest(as.character(marathon_best_fit$fit$actions$model$spec$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
        expect_true('nearest_neighbor' %in% class(marathon_best_fit$fit$actions$model$spec))
    })
    test_that('marathon_best_fit does not contain the correct recipe', {
        expect_true('recipe' %in% class(marathon_best_fit$pre$actions$recipe$recipe))
        expect_equal(digest(int_round(sum(marathon_best_fit$pre$actions$recipe$recipe$template$max), 0)), '94e91e5e6573ddfedb81802729c39543')
        expect_equal(digest(int_round(sum(marathon_best_fit$pre$actions$recipe$recipe$template$time_hrs), 0)), '2213c3a0eb86305be22e0ca3b0a773c1')
    })
    test_that('Did not create an object named marathon_summary', {
        expect_true(exists('marathon_summary'))
    })
    test_that('marathon_summary is not a tibble', {
        expect_true('tbl' %in% class(marathon_summary))
    })
    test_that('marathon_summary contains the incorrect data', {
        expect_true('.metric' %in% colnames(marathon_summary))
        expect_true('.estimator' %in% colnames(marathon_summary))
        expect_true('.estimate' %in% colnames(marathon_summary))
        expect_equal(digest(int_round(sum(marathon_summary$.estimate), 0)), '4b5630ee914e848e8d07221556b0a2fb')
    })
    print("Success!")
}

test_8.3 <- function(){
    test_that('Did not create an objected named answer8.3', {
        expect_true(exists('answer8.3'))
    })
    test_that('answer is incorrect', {
        expect_equal(digest(answer8.3), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
    print("Success!")
}

test_9.0 <- function(){
    properties <- c(marathon_plot$layers[[1]]$mapping, marathon_plot$mapping)
    labels <- marathon_plot$labels
    test_that('Did not create an object named marathon_preds', {
        expect_true(exists('marathon_preds')) 
    })
    test_that('marathon_preds should be a tibble', {
        expect_true('tbl' %in% class(marathon_preds))
    })
    test_that('marathon_preds contains incorrect data', {
        expect_equal(dim(marathon_preds), c(698, 14))
        expect_true('.pred' %in% colnames(marathon_preds))
        expect_equal(digest(int_round(sum(marathon_preds$.pred), 2)), 'f5847c263ec9596eee1ae122cdd1e347')
        expect_equal(digest(int_round(sum(marathon_preds$time_hrs), 2)), 'b9b7909060cc50e65fb7ff452897a2b4') 
    })
    test_that('Did not create an object called marathon_plot', {
        expect_true(exists('marathon_plot'))
    })
     test_that('max should be on the x-axis.', {
        expect_true("max" == rlang::get_expr(properties$x))
        })
    test_that('time_hrs should be on the y-axis.', {
        expect_true("time_hrs" == rlang::get_expr(properties$y))
        })
    test_that('marathon_plot should have full_predictions plotted as a blue line over the data points.', {
        expect_true('blue' %in% as.character(marathon_plot$layers[[2]]$aes_params))
        expect_true('GeomLine' %in% c(class(rlang::get_expr(marathon_plot$layers[[1]]$geom)), class(rlang::get_expr(marathon_plot$layers[[2]]$geom))))
    })
    test_that('max should be the x argument for geom_line', {
        expect_true('max' == rlang::get_expr(marathon_plot$layers[[2]]$mapping$x))
    })
    test_that('.pred should be the y argument for geom_line',{
        expect_true('.pred' == rlang::get_expr(marathon_plot$layers[[2]]$mapping$y))
    })
    test_that('Labels on the axes/title and legend need to be changed to be descriptive, nicely formatted, and human readable.', {
        expect_false((labels$y) == 'time_hrs')
        expect_false((labels$x) == 'max')
        expect_false((labels$title == 'k_min'))
        })
print("Success!")
}
