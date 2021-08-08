library(testthat)
library(digest)

# Round double to precise integer
#
# `int_round` works to create an integer corresponding to a number that is 
# tested up to a particular decimal point of precision. This is useful when 
# there is a need to compare a numeric value using hashes.
#
# @param x Double vector of length one.
# @param digits Double vector of length one to specify decimal point of precision. Negative numbers can be used to specifying significant digits > 0.1.
#
# @return Integer vector of length one corresponding to a particular decimal point of precision.
#
# @examples
# # to get an integer up to two decimals of precision from 234.56789
# int_round(234.56789, 2)
#
# to get an integer rounded to the hundred digit from 234.56789
# int_round(234.56789, -2)
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
    test_that('Did not create an object named pm_data', {
    expect_true(exists("pm_data")) 
})
test_that('pm_data should be a tibble.', {
    expect_true('tbl' %in% class(pm_data))
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
    expect_equal(int_round(nrow(pm_pairs$data), 0), 800)
    expect_equal(int_round(ncol(pm_pairs$data), 0), 7)
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
    test_that('km_data should contain the columns Speed and Defense', {
    expect_true('Speed' %in% colnames(km_data))
    expect_true('Defense' %in% colnames(km_data))
    })
test_that('km_data should contain 800 rows and 2 columns.', {
    expect_equal(int_round(ncol(km_data), 0), 2)
    expect_equal(int_round(nrow(km_data), 0), 800)
    })
print("Success!")
    }

test_1.3 <- function(){
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
        expect_true("GeomPoint" %in% c(class(pm_scatter$layers[[1]]$geom)))
        })
    test_that('Labels on the axes and legend need to be changed to be descriptive, nicely formatted, and human readable.', {
        expect_false((labels$y) == 'Defense')
        expect_false((labels$x) == 'Speed')
        })
print("Success!")
    }

test_1.4.2 <- function(){
    test_that('The pokemon_clusters model should have 4 centers.', {
    expect_equal(int_round(nrow(pokemon_clusters$centers), 0), 4)
    })
test_that('The pokemon_clusters model should be using Speed and Defense to create the clusters.', {
    expect_equal(int_round(ncol(pokemon_clusters$centers), 0), 2)
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
        expect_true("GeomPoint" %in% c(class(answer1.5$layers[[1]]$geom)))
        })
    test_that('Labels on the axes and legend need to be changed to be descriptive, nicely formatted, and human readable.', {
        expect_false((labels$y) == 'Defense')
        expect_false((labels$x) == 'Speed')
        expect_false((labels$colour) == '.cluster')
        })
print("Success!")
    }

test_1.7 <- function(){
test_that('elbow_stats should contain k from 1 to 10', {
    expect_equal(int_round(nrow(elbow_stats), 0), 10)
    })
test_that('Solution is incorrect', {
    expect_equal(digest(int_round(sum(elbow_stats$tot.withinss), 2)), 'ad3673e6aed949387c2f1f4d4b9141dc')
    })
test_that('poke_clusts column should be removed', {
    expect_false("poke_clusts" %in% colnames(elbow_stats))
    })    
print("Success!")
    }

test_1.8 <- function(){
properties <- c(elbow_plot$layers[[1]]$mapping, elbow_plot$mapping)
properties2 <- c(elbow_plot$later[[2]]$mapping, elbow_plot$mapping)
test_that('Did not create a plot called elbow_plot', {
    expect_true(exists('elbow_plot'))
})
test_that('elbow_plot should be a line plot with points', {
    expect_true("GeomPoint" %in% c(class(elbow_plot$layers[[1]]$geom), class(elbow_plot$layers[[2]]$geom)))
    expect_true("GeomLine" %in% c(class(elbow_plot$layers[[1]]$geom), class(elbow_plot$layers[[2]]$geom)))
})
test_that('k should be on the x-axis', {
    expect_true(rlang::get_expr(properties$x) == 'k')
})
test_that('tot.withinss should be on the y-axis', {
    expect_true(rlang::get_expr(properties$y) == 'tot.withinss')
})
test_that('Labels on the axes should be descriptive and human readable.', {
       # expect_false((elbow_plot$labels$x) == 'k')
        expect_false((elbow_plot$labels$y) == 'tot.withinss')
})
print('Success!')
}
