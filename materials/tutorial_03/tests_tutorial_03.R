library(testthat)
library(digest)

test_0.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(A), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(B), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(C), '6717f2823d3202449301145073ab8719') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(D), 'dbc09cba9fe2583fb01d63c70e1555a8') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(E), '0aee9b78301d7ec8998971363be87c03') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(F), '5e338704a8e069ebd8b38ca71991cf94') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.1 <- function(){
    test_that('Did not create an object named avocado', {
        expect_true(exists("avocado")) 
        })
    test_that('avocado should be a data frame.', {
        expect_true('data.frame' %in% class(avocado))
        })
    test_that('avocado does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(avocado), c(17911, 9))
        })
    test_that('avocado does not contain the correct data.', {
        expect_equal(digest(sum(avocado$average_price)), 'b987e782cc203bbe06ef8fde8da42991') 
        expect_equal(colnames(avocado), c("Date", "average_price", "small_hass_volume", "large_hass_volume", "extra_l_hass_volume", "type", "year", "region", "week"))
        })
print("Success!")
    }

test_1.2 <- function(){
    test_that('Did not create an object named cheapest', {
        expect_true(exists("cheapest")) 
        })
    test_that('cheapest should be a data frame.', {
        expect_true('data.frame' %in% class(cheapest))
        })
    test_that('avocado does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(cheapest), c(1, 2))
        })
    test_that('cheapest does not contain the correct data.', {
        expect_equal(digest(cheapest$region[0]), '5152ac13bdd09110d9ee9c169a3d9237') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(as.numeric(unlist(select(cheapest, -region)))), '481ea83b5704f345de5f42f139ee11c7') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.3 <- function(){
    test_that('Did not create a plot named avocado_plot', {
        expect_true(exists("avocado_plot")) 
        })
    test_that('total_volume should be on the x-axis.', {
        expect_that("total_volume" %in% c(rlang::get_expr(avocado_plot$mapping$x),rlang::get_expr(avocado_plot$layers[[1]]$mapping$x)), is_true())
        })
    test_that('average_price should be on the y-axis.', {
        expect_that("average_price" %in% c(rlang::get_expr(avocado_plot$mapping$y), rlang::get_expr(avocado_plot$layers[[1]]$mapping$y)) , is_true())
        })
    test_that('avocado_plot should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(avocado_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((avocado_plot$labels$y) == 'average_price', is_false())
        expect_that((avocado_plot$labels$x) == 'total_volume', is_false())
        })
print("Success!")
    }

test_3.1 <- function(){
    test_that('Did not create an object named sea_surface', {
        expect_true(exists("sea_surface")) 
        })
    test_that('sea_surface should be a data frame.', {
        expect_true('data.frame' %in% class(sea_surface))
        })
    test_that('sea_surface does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(sea_surface), c(105, 13))
        })
    test_that('sea_surface does not contain the correct data.', {
        expect_equal(digest(sum(sea_surface$Dec)), '9c9393e1464352cd4fbea94dfadfa02a') 
        expect_equal(colnames(sea_surface), c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        })
print("Success!")
    }