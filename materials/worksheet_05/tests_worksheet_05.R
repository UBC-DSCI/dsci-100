# +
library(testthat)
library(digest)

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

test_1.1 <- function(){
    test_that('Solution is incorrect.', {
        expect_equal(digest(answer1.1), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_1.2 <- function(){
    test_that('Solution is incorrect. Git is a tool for version control that is used locally on your computer.', {
        expect_equal(digest(answer1.2), 'd2a90307aac5ae8d0ef58e2fe730d38b') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success! Git is a tool for version control that is used locally on your computer, whereas GitHub is an example of a remote/cloud repository hosting service where you can backup and share your files with collaborators.")
}

test_2.1 <- function(){
    test_that('Solution is incorrect.', {
        expect_equal(digest(answer2.1), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_3.1 <- function(){
    test_that('Solution is incorrect. Technically, you can write any kind of message you want, but they are only useful if they describe what the change to the file(s) was about!', {
        expect_equal(digest(answer3.1), 'd2a90307aac5ae8d0ef58e2fe730d38b') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success! Yes commit messages are required, and yes what is in the message is important! The most useful messages describe what the change to the file(s) was about so that you can easily and effectively review the project's history!")
}

test_4.1 <- function(){
    test_that('Solution is incorrect.', {
        expect_equal(digest(answer4.1), '05ca18b596514af73f6880309a21b5dd') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_6.1 <- function(){
    test_that('Solution is incorrect.', {
        expect_equal(digest(answer6.1), '01a75cb73d67b0f895ff0e61449c7bf8') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_7.1 <- function(){
    test_that('Solution is incorrect. Committing your changes only puts them in the Git history on the local computer you are working on (i.e., your workspace on the JupyterHub or your laptop). To get the changes on GitHub you need to do an additional step of pushing the changes to the remote repository on GitHub.', {
        expect_equal(digest(answer7.1), 'd2a90307aac5ae8d0ef58e2fe730d38b') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success! You're right! The changes (and all the associated information) are not yet on GitHub, they are only in the Git history on the local computer you are working on (i.e., your workspace on the JupyterHub or your laptop).")
}

test_8.1 <- function(){
    test_that('Solution is incorrect.', {
        expect_equal(digest(answer8.1), '75f1160e72554f4270c809f041c7a776') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_9.1 <- function(){
    test_that('Solution is incorrect. Public repositories are viewable by anyone, but not editable by everyone.', {
        expect_equal(digest(answer9.1), '05ca18b596514af73f6880309a21b5dd') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success! Nice work! You can share your work publicly on GitHub while still retaining control over who can edit the shared work.")
}

test_10.1 <- function(){
    test_that('Solution is incorrect.', {
        expect_equal(digest(answer10.1), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}
