library(testthat)
library(digest)


test_1.0 <- function(){
    test_that('Solution is incorrect', {
    expect_that(exists('answer1.0'), is_true())
    expect_equal(digest(answer1.0), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }


test_1.1 <- function(){
test_that('Solution is incorrect', {
    expect_that(exists('answer1.1'), is_true())
    expect_equal(digest(answer1.1), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_2.0 <- function(){
test_that('number_of_rows does not exist', {
    expect_that(exists('number_of_rows'), is_true())
    })
test_that('The number of rows is incorrect', {
    expect_equal(digest(number_of_rows), '9ecacdf4df8df05e2501eb3f18bd13ab') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(nrow(counts), 10)
    expect_equal(sum(as.numeric(counts$y)), 55)
    expect_equal(dim(counts), c(10, 2))
})
print("Success!")
    }

test_2.4 <- function(){
test_that('Solution is incorrect', {
    expect_equal(digest(label_102[[1]]), '7951b4c16d8f371c0ff9d97401f46907') # we hid the answer to the test here so you can't see it, but we can still run the test
    
})
print("Success!")
    }
