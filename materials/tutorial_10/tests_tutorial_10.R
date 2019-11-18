library(testthat)
library(digest)

test_1.0 <- function(){
    test_that('Did not create an object named pm_data', {
    expect_true(exists("pm_data")) 
})
test_that('pm_data should be a data frame.', {
    expect_true('data.frame' %in% class(pm_data))
})
test_that('pm_data does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(pm_data), c(800, 13))
})
test_that('pm_data is missing columns.', {
    expect_true('Name' %in% colnames(pm_data))
    expect_true('HP' %in% colnames(pm_data))
    expect_true('Attack' %in% colnames(pm_data))
    expect_true('Defense' %in% colnames(pm_data))
    expect_true('#' %in% colnames(pm_data))
    expect_true('Type 1' %in% colnames(pm_data))
    expect_true('Type 2' %in% colnames(pm_data))
    expect_true('Total' %in% colnames(pm_data))
    expect_true('Sp. Atk' %in% colnames(pm_data))
    expect_true('Sp. Def' %in% colnames(pm_data))
    expect_true('Speed' %in% colnames(pm_data))
    expect_true('Generation' %in% colnames(pm_data))
    expect_true('Legendary' %in% colnames(pm_data))
    })
print("Success!")
    }

test_1.1 <- function(){
    test_that('Did not create a plot named pm_pairs', {
    expect_true(exists("pm_pairs")) 
})
test_that('pm_pairs should be using data from pm_data', {
    expect_equal(nrow(pm_pairs$data), 800)
    expect_equal(ncol(pm_pairs$data), 7)
})
test_that('pm_pairs should be a pairwise plot matrix.', {
    expect_true('ggmatrix' %in% c(class(pm_pairs)))
    })
test_that('pm_pairs should plot columns 5 to 11', {
    expect_equal(pm_pairs$yAxisLabels %in% c("Total", "HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed"), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
    })
print("Success!")
    }

test_1.2 <- function(){
    properties <- c(pm_scatter$layers[[1]]$mapping, pm_scatter$mapping)
    labels <- pm_scatter$labels
    test_that('Did not create a plot named pm_scatter', {
        expect_true(exists("pm_scatter")) 
        })
    test_that('Speed should be on the x-axis.', {
        expect_true("Speed" == rlang::get_expr(properties$x))
        })
    test_that('Defense should be on the y-axis.', {
        expect_true("Defense" == rlang::get_expr(properties$y))
        })
    test_that('pm_scatter should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(pm_scatter$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes and legend need to be changed to be descriptive, nicely formatted, and human readable.', {
        expect_that((labels$y) == 'Defense', is_false())
        expect_that((labels$x) == 'Speed', is_false())
        })
print("Success!")
    }



test_1.3 <- function(){
    test_that('km_data should contain the columns Speed and Defense', {
    expect_true('Speed' %in% colnames(km_data))
    expect_true('Defense' %in% colnames(km_data))
    })
test_that('km_data should contain 800 rows and 2 columns.', {
    expect_equal(ncol(km_data), 2)
    expect_equal(nrow(km_data), 800)
    })
print("Success!")
    }

test_1.4.2 <- function(){
    test_that('The pokemon_clusters model should have 4 centers.', {
    expect_equal(nrow(pokemon_clusters$centers), 4)
    })
test_that('The pokemon_clusters model should be using Speed and Defense to create the clusters.', {
    expect_equal(ncol(pokemon_clusters$centers), 2)
    expect_true('Speed' %in% colnames(pokemon_clusters$centers))
    expect_true('Defense' %in% colnames(pokemon_clusters$centers))
    })
test_that('The pokemon_clusters model should be of class kmeans', {
    expect_equal(class(pokemon_clusters), 'kmeans')
    })
print("Success!")
    }

test_1.5 <- function(){
    properties <- c(answer1.5$layers[[1]]$mapping, answer1.5$mapping)
    labels <- answer1.5$labels
    test_that('Did not create a plot named answer1.5', {
        expect_true(exists("answer1.5")) 
        })
    test_that('Speed should be on the x-axis.', {
        expect_true("Speed" == rlang::get_expr(properties$x))
        })
    test_that('Defense should be on the y-axis.', {
        expect_true("Defense" == rlang::get_expr(properties$y))
        })
    test_that('answer1.5 should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(answer1.5$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes and legend need to be changed to be descriptive, nicely formatted, and human readable.', {
        expect_that((labels$y) == 'Defense', is_false())
        expect_that((labels$x) == 'Speed', is_false())
        expect_that((labels$colour) == '.cluster', is_false())
        })
print("Success!")
    }

test_1.7 <- function(){
    test_that('elbow_stats should contain k from 1 to 10', {
    expect_equal(nrow(elbow_stats), 10)
    })
test_that('Solution is incorrect', {
    expect_equal(sum(c('k', 'tot.withinss') %in% colnames(elbow_stats)), 2)
    })
print("Success!")
    }