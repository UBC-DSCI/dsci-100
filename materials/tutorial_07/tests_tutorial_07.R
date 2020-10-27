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


test_1.0 <- function(){
    test_that('Solution is incorrect', {
    expect_true(exists('answer1.0'))
    expect_equal(digest(answer1.0), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
})
print("Success!")
    }        

test_1.1 <- function(){
test_that('Solution is incorrect', {
    expect_true(exists('answer1.1'))
    expect_equal(digest(answer1.1), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
})
print("Success!")
    }    

test_2.0 <- function(){
test_that('number_of_rows does not exist', {
    expect_true(exists('number_of_rows'))
    })
test_that('The number of rows is incorrect', {
    expect_equal(digest(int_round(number_of_rows, 0)), 'a58969729b9f8713e03743154dfa7b57') # we hid the answer to the test here so you can't see it, but we can still run the test
    })
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(int_round(nrow(counts), 0), 10)
    expect_equal(digest(int_round(sum(as.integer(counts$y)), 2)), 'ae80db6ce6693e364a09f88dd4e3d553')
    expect_equal(dim(counts), c(10, 2))
    expect_equal(digest(int_round(sum(as.integer(counts$n)), 2)), '9ecacdf4df8df05e2501eb3f18bd13ab')
})
print("Success!")
    }

test_2.2 <- function(){
test_that('answer2.2 does not exist', {
    expect_true(exists('answer2.2'))
})
test_that('Solution is incorrect', {
    expect_equal(digest(answer2.2), 'd2a90307aac5ae8d0ef58e2fe730d38b')
    })
print("Success!")
    }

test_2.4 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(int_round(as.integer(label_102[[1]]), 2)), '069c0949cc402f30063b90e9a31f87e1') # we hid the answer to the test here so you can't see it, but we can still run the test
    })
print("Success!")
    }

test_3.0 <- function(){
test_that('testing_set does not exist', {
        expect_true(exists('testing_set'))
})
test_that('testing_set does not contain the correct data', {
        expect_equal(dim(testing_set), c(600, 786)) 
})
test_that('training_set does not exist', {
        expect_true(exists('training_set'))
})
test_that('training_set does not contain the correct data',{
        expect_equal(dim(training_set), c(600, 785))
})
print("Success!")
    }

test_3.1 <- function(){
test_that('answer3.1 does not exist', {
        expect_true(exists('answer3.1'))
})
test_that('answer3.1 is incorrect', {
        expect_equal(digest(answer3.1), '75f1160e72554f4270c809f041c7a776')
})
print("Success!")
    }

test_3.2 <- function(){
 properties <- c(cross_val_plot$layers[[1]]$mapping, cross_val_plot$mapping)
    labels <- cross_val_plot$labels
test_that('Did not create an object named cross_val_plot',{
        expect_true(exists('cross_val_plot'))
})
test_that('cross_val_plot is not a lineplot', {
        expect_true('GeomPoint' %in% class(cross_val_plot$layers[[1]]$geom))
        expect_true('GeomLine' %in% class(cross_val_plot$layers[[2]]$geom))
})
test_that('neighbors should be on the x-axis.', {
        expect_true("neighbors" == rlang::get_expr(properties$x))
        })
    test_that('mean should be on the y-axis.', {
        expect_true("mean" == rlang::get_expr(properties$y))
        })
test_that('cross_val_plot does not contain the correct data.', {
        expect_equal(digest(int_round(sum(cross_val_plot$data$mean), 2)), '088459b2025cd3f9cbf1451f8c2c1636')
        expect_equal(digest(int_round(sum(cross_val_plot$data$n), 2)), '189e2f1b2fbb3743811990e9708c226a')
})
test_that('cross_val_plot should only contain accuracy.', {
        expect_true('accuracy' %in% unique(cross_val_plot$data$.metric))
})
print("Success!")
    }

test_3.3 <- function(){
test_that('Did not create an object named answer3.3', {
    expect_true(exists('answer3.3'))
})
test_that('Solution is incorrect', {
    expect_equal(digest(answer3.3), '75f1160e72554f4270c809f041c7a776')
})
print("Success!")
    }

test_4.0 <- function(){
test_that('Did not create an object named mnist_spec', {
    expect_true(exists("mnist_spec"))
    })
test_that('k value is incorrect', {
    expect_equal(digest(int_round(get_expr(mnist_spec$args$neighbors), 0)), '11946e7a3ed5e1776e81c0f0ecd383d0')
    })
test_that('weight_func is incorrect', {
    expect_equal(digest(as.character(get_expr(mnist_spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    })
test_that('set_engine is incorrect', {
    expect_equal(digest(as.character(mnist_spec$engine)), '93fe1d3f0a1fa2e625af1e1eb51a5c33')
    })
test_that('mode is incorrect', {
    expect_equal(digest(as.character(mnist_spec$mode)), 'f361ba6f6b32d068e56f61f53d35e26a')
    })
test_that('Did not create an object named mnist_fit', {
    expect_true(exists("mnist_fit")) 
    })
test_that('mnist_fit should be a fit model.', {
    expect_true('workflow' %in% class(mnist_fit))
    })
test_that('mnist_fit does not include training_set dataset', {
    expect_equal(digest(int_round(sum(as.integer(unlist(mnist_fit$fit$fit$fit$data[785]))), 2)), '4eb0b7cdd24a983324274d66e0860365')
    })
test_that('mnist_fit does not contain mnist_spec', {
    expect_equal(digest(int_round(get_expr(mnist_fit$fit$fit$spec$args$neighbors), 0)), '11946e7a3ed5e1776e81c0f0ecd383d0')
    expect_equal(digest(as.character(get_expr(mnist_fit$fit$fit$spec$args$weight_func))), '989de78e881829b4499af3610dfe54fd')
    expect_equal(digest(as.character(mnist_fit$fit$fit$spec$mode)), 'f361ba6f6b32d068e56f61f53d35e26a')
    })
print("Success!")
    }

test_4.1 <- function(){
test_that('Did not create an object named mnist_predictions', {
    expect_true(exists('mnist_predictions'))
})
test_that('mnist_predictions does not contain the correct data', {
    expect_equal(digest(int_round(sum(as.integer(mnist_predictions$.pred_class)), 2)), 'ad1b496bd820958116aa67cdb147352b')
    expect_equal(digest(int_round(sum(as.integer(mnist_predictions$y)), 2)), '733395e0768b9479c1feffa0b067fcdd')
})
test_that('Did not create an objected named training_results', {
    expect_true(exists('training_results'))
})
test_that('training_results is not a confusion matrix', {
    expect_true('conf_mat' %in% class(training_results))
})
test_that('training_results contains the incorrect data', {
    expect_equal(digest(int_round(sum(as.integer(training_results$table)), 2)), '9ecacdf4df8df05e2501eb3f18bd13ab')
})
print("Success!")
    }

test_4.4 <- function(){
test_that('Did not create an object named answer4.4', {
    expect_true(exists('answer4.4'))
})
test_that('Solution is incorrect', {
    expect_equal(digest(answer4.4), '05ca18b596514af73f6880309a21b5dd')
})
print("Success!")
    }

test_4.5 <- function(){
test_that('Did not create an object named answer4.5', {
    expect_true(exists('answer4.5'))
})
test_that('Solution is incorrect', {
    expect_equal(digest(answer4.5), '3a5505c06543876fe45598b5e5e5195d')
})
print("Success!")
    }
