library(testthat)
library(digest)

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
        expect_true(digest(round(answer0.2, 2)) %in% c('651ba44efc6a75d694ff482aae958ccc', '2a1ea47875e195a421d56ae3f6621d32'))
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
    test_that('marathon should be a data frame.', {
        expect_true('data.frame' %in% class(marathon))
    })
    test_that('marathon does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(marathon), c(929, 13))
    })
    test_that('The marathon data frame is missing columns.', {
        expect_that("time_hrs" %in% colnames(marathon), is_true())
        expect_that("max" %in% colnames(marathon), is_true())
    })
    print("Success!")
}

test_2.0 <- function(){
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
        expect_that("max" %in% c(rlang::get_expr(answer2$mapping$x),
                                 rlang::get_expr(answer2$layers[[1]]$mapping$x)), is_true())
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_that("time_hrs" %in% c(rlang::get_expr(answer2$mapping$y), 
                                      rlang::get_expr(answer2$layers[[1]]$mapping$y)) , is_true())
    })
    test_that('answer2 should be a scatter plot.', {
        expect_equal(digest(class(rlang::get_expr(answer2$layers[[1]]$geom))[1]), 
                 '911e5b9debfb523f25ad2ccc01a4b2dd')
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((answer2$labels$y) == 'time_hrs', is_false())
        expect_that((answer2$labels$x) == 'max', is_false())
    })
print("Success!")  
}

test_3.0 <- function(){
    test_that('answer3 is incorrect', {
        expect_true(exists('answer3'))
        expect_equal(digest(round(answer3[[1]], 1)), 'b0370aa0feee1e438e74c77cd361ae86')
})
    print("Success!")
}

test_4.0 <- function(){
    test_that('answer4 is incorrect', {
        expect_true(exists('answer4'))
        expect_equal(digest(round(answer4[[1]], 1)), '316c6aa53246a0e994e472fb4af30fc0')
    })
    print("Success!")
}

test_5.0 <- function(){
    test_that('Solution is incorrect', {
        expect_that(exists('answer5'), is_true())
        expect_equal(digest(answer5), '475bf9280aab63a82af60791302736f6')
    })
    print("Success!")
}

test_6.0 <- function(){
    test_that('Did not create an object named training_rows', {
        expect_true(exists("training_rows")) 
    })
    test_that('training_rows should be a matrix (not a data frame)', {
        expect_true('matrix' %in% class(training_rows))
    })
    test_that('training_rows is not the correct length.', {
        expect_equal(length(training_rows), 698)
    })
    print("Success!")
}

test_7.0 <- function(){
    test_that('Did not create an object named X_train', {
        expect_true(exists("X_train")) 
    })
    test_that('X_train should be a data frame.', {
        expect_true('data.frame' %in% class(X_train))
        })
    test_that('X_train does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(X_train), c(698, 1))
        })
    test_that('X_train does not contain the column max', {
        expect_true('max' %in% colnames(X_train))
    })
    test_that('Did not create an object named X_test', {
        expect_true(exists("X_test")) 
    })
    test_that('X_test should be a data frame.', {
        expect_true('data.frame' %in% class(X_test))
    })
    test_that('X_test does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(X_test), c(231, 1))
    })
    test_that('X_test does not contain the column max', {
        expect_true('max' %in% colnames(X_test))
    })
    test_that('Did not create an object named Y_train', {
        expect_true(exists("Y_train")) 
    })
    test_that('Y_train should be a numeric list', {
        expect_true('numeric' %in% class(Y_train))
    })
    test_that('Y_train is not the correct length.', {
        expect_equal(length(Y_train), 698)
    })
    test_that('Did not create an object named Y_test', {
        expect_true(exists("Y_test")) 
    })
    test_that('Y_test should be a numeric list', {
        expect_true('numeric' %in% class(Y_test))
    })
    test_that('Y_test is not the correct length.', {
        expect_equal(length(Y_test), 231)
    })
print("Success!")
}

test_8.0 <- function(){
    test_that('Did not create an object named knn_cv', {
        expect_true(exists("knn_cv")) 
    })
    test_that('x in knn_cv should be X_train', {
        expect_equal(as.character(knn_cv$call$x), 'X_train')
    })
    test_that('y in knn_cv should be Y_train', {
        expect_equal(as.character(knn_cv$call$y), 'Y_train')
    })
    test_that('method should be knn', {
        expect_equal(as.character(knn_cv$method), 'knn')
    })
    test_that('k should be chosen from k_lots', {
        expect_equal(as.numeric(sum(knn_cv$results$k)), 1065)
    })
    test_that('knn_cv should use 3 fold cross-validation.', {
        expect_equal(knn_cv$control$method, 'cv')
        expect_equal(knn_cv$control$number, 3)
    })
print("Success!")
}


test_8.1 <- function(){
    test_that('Did not create a plot named choosing_k', {
        expect_true(exists("choosing_k")) 
    })
    test_that('The plot should be based on the knn_cv object.', {
        expect_equal(choosing_k$data$k, knn_cv$results$k)
        expect_equal(choosing_k$data$RMSE, knn_cv$results$RMSE)
    })
    test_that('k should be on the x-axis.', {
        expect_that("k" %in% c(rlang::get_expr(choosing_k$mapping$x),rlang::get_expr(choosing_k$layers[[1]]$mapping$x)), is_true())
    })
    test_that('RMSE should be on the y-axis.', {
        expect_that("RMSE" %in% c(rlang::get_expr(choosing_k$mapping$y), rlang::get_expr(choosing_k$layers[[1]]$mapping$y)) , is_true())
    })
    test_that('choosing_k should be a scatter plot and a line plot.', {
        expect_that('GeomPoint' %in% c(class(rlang::get_expr(choosing_k$layers[[1]]$geom)), class(rlang::get_expr(choosing_k$layers[[2]]$geom))), is_true())
    expect_that('GeomLine' %in% c(class(rlang::get_expr(choosing_k$layers[[1]]$geom)), class(rlang::get_expr(choosing_k$layers[[2]]$geom))), is_true())
    })
    print("Success!")
}

test_8.2 <- function(){
    test_that('best_k is incorrect', {
        expect_equal(digest(as.numeric(best_k[[1]])), 'dc65e64a0fb0e6b4c47b0ae79cff0459')
    })
    print("Success!")
}

test_9.0 <- function(){
    test_that('Did not create an object named knn_model', {
        expect_true(exists("knn_model")) 
    })
    test_that('x in knn_model should be X_train', {
        expect_equal(as.character(knn_model$call$x), 'X_train')
    })
    test_that('y in knn_model should be Y_train', {
        expect_equal(as.character(knn_model$call$y), 'Y_train')
    })
    test_that('method should be knn', {
        expect_equal(as.character(knn_model$method), 'knn')
    })
    test_that('k should be 75', {
        expect_equal(as.integer(knn_model$results$k), 51)
    })
    test_that('knn_model contains incorrect trainingData.', {
        expect_equal(colnames(knn_model$trainingData), c('max', '.outcome'))
        expect_equal(dim(knn_model$trainingData), c(698, 2))
    })
print("Success!")
}

test_10.0 <- function(){
    test_that('predictions should be a numeric vector or length 231', {
        expect_true(class(predictions) == 'numeric')
        expect_equal(length(predictions), 231)
        expect_equal(digest(mean(predictions)), '71ba098ce630c0890084dbe1ae18b8e8')
    })
    test_that('values of predictions are incorrect', {
        expect_equal(digest(mean(predictions)), '71ba098ce630c0890084dbe1ae18b8e8')
    })
    print("Success!")
}

test_11.0 <- function(){
    test_that('test_error is incorrect', {
        expect_true('e2a1b82452e5d815ae7be864f8ba1ef1' %in% map(round(test_error, 3), digest) )
    })
    print("Success!")
}

test_11.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer11.1), '05ca18b596514af73f6880309a21b5dd')
    })
print("Success!")    
}

test_11.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer11.2), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
print("Success!") 
}

test_12.0 <- function(){
    test_that('Did not create an object named full_predictions', {
        expect_true(exists("full_predictions")) 
    })
    test_that('full_predictions should be a data frame.', {
        expect_true('data.frame' %in% class(full_predictions))
    })
    test_that('full_predictions does not contain time_hrs and/or max', {
        expect_true('time_hrs' %in% colnames(full_predictions))
        expect_true('max' %in% colnames(full_predictions))
    })
    test_that('full_predictions contains incorrect information.', {
        expect_equal(round(sum(full_predictions$time_hrs)), 434)
    })
    print("Success!")
}

test_13.0 <- function(){
    test_that('Did not create a plot named predict_plot', {
        expect_true(exists("predict_plot")) 
    })
    test_that('max should be on the x-axis.', {
        expect_that("max" %in% c(rlang::get_expr(predict_plot$mapping$x),rlang::get_expr(predict_plot$layers[[1]]$mapping$x)), is_true())
    })
    test_that('time_hrs should be on the y-axis.', {
        expect_that("time_hrs" %in% c(rlang::get_expr(predict_plot$mapping$y), rlang::get_expr(predict_plot$layers[[1]]$mapping$y)) , is_true())
    })
    test_that('predict_plot should be a scatter plot.', {
        expect_true('GeomPoint' %in% c(class(rlang::get_expr(predict_plot$layers[[1]]$geom)), class(rlang::get_expr(predict_plot$layers[[2]]$geom))))
    })
    test_that('predict_plot should have full_predictions plotted as a blue line over the data points.', {
        expect_true('GeomLine' %in% c(class(rlang::get_expr(predict_plot$layers[[1]]$geom)), class(rlang::get_expr(predict_plot$layers[[2]]$geom))))
    })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((answer2$labels$y) == 'time_hrs', is_false())
        expect_that((answer2$labels$x) == 'max', is_false())
    })
    print("Success!")
}







