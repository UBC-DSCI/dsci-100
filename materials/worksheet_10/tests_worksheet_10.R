# +
library(testthat)
library(digest)

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

test_0.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.0), '01a75cb73d67b0f895ff0e61449c7bf8') 
    })
    print("Success!")
}
test_0.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.1), 'd19d62a873f08af0488f0df720cfd293') 
    })
    print("Success!")
    }

test_1.0 <- function(){
    test_that('Did not create an object named beer', {
        expect_true(exists("beer")) 
    })
    test_that('beer should be a tibble.', {
        expect_true('tbl' %in% class(beer))
    })
    test_that('beer does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(beer), c(2410, 8))
    })
    test_that('The beer tibble is missing columns.', {
        expect_true("abv" %in% colnames(beer))
        expect_true("ibu" %in% colnames(beer))
        expect_true("id" %in% colnames(beer))
        expect_true("name" %in% colnames(beer))
        expect_true("style" %in% colnames(beer))
        expect_true("brewery_id" %in% colnames(beer))
        expect_true("ounces" %in% colnames(beer))
    })
    print("Success!")
}

test_1.1 <- function(){
    properties <- c(beer_eda$layers[[1]]$mapping, beer_eda$mapping)
    labels <- beer_eda$labels    
    test_that('Did not create a plot named beer_eda', {
        expect_true(exists("beer_eda")) 
    })
    test_that('ibu should be on the x-axis.', {
        expect_true("ibu" == rlang::get_expr(properties$x))
        })
    test_that('abv should be on the y-axis.', {
        expect_true("abv" == rlang::get_expr(properties$y))
        })
    test_that('beer_eda should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(beer_eda$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false((labels$y) == 'abv')
        expect_false((labels$x) == 'ibu')
        })
    print("Success!")
}

test_1.2 <- function(){
    test_that('Did not create an object named clean_beer', {
        expect_true(exists("clean_beer")) 
    })
    test_that('clean_beer should be a tibble.', {
        expect_true('tbl' %in% class(clean_beer))
    })
    test_that('clean_beer should only contain the columns ibu and abv', {
        expect_true("ibu" %in% colnames(clean_beer))
        expect_true("abv" %in% colnames(clean_beer))
        expect_false("id" %in% colnames(clean_beer))
        expect_false("name" %in% colnames(clean_beer))
        expect_false("style" %in% colnames(clean_beer))
        expect_false("brewery_id" %in% colnames(clean_beer))
        expect_false("ounces" %in% colnames(clean_beer))
        })
    test_that('clean_beer does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(clean_beer), c(1405, 2))
    })

    print("Success!")
    }

test_1.3.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.3.1), '75f1160e72554f4270c809f041c7a776') 
    })
    print("Success!")
    }

test_1.3.2 <- function(){
    test_that('Did not create an object named scaled_beer', {
        expect_true(exists("scaled_beer")) 
    })
    test_that('scaled_beer should be a tibble.', {
        expect_true('tbl' %in% class(scaled_beer))
    })
    test_that('scaled_beer does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(scaled_beer), c(1405, 2))
    })
    test_that('scaled_beer should only contain the columns ibu and abv', {
        expect_true("ibu" %in% colnames(clean_beer))
        expect_true("abv" %in% colnames(clean_beer))
        expect_false("id" %in% colnames(clean_beer))
        expect_false("name" %in% colnames(clean_beer))
        expect_false("style" %in% colnames(clean_beer))
        expect_false("brewery_id" %in% colnames(clean_beer))
        expect_false("ounces" %in% colnames(clean_beer))
        })
    test_that('Columns in scaled_beer are not scaled correctly.', {
        expect_true(min(scaled_beer$ibu) < 1)
        expect_true(max(scaled_beer$ibu) < 4)
        expect_true(min(scaled_beer$abv) < -2)
        expect_true(max(scaled_beer$abv) < 5)
        })
    print("Success!")
}

test_1.4 <- function(){
    test_that('beer_cluster_k2 class should be kmeans', {
        expect_equal(class(beer_cluster_k2), 'kmeans')
        })
    test_that('beer_cluster_k2 should have 2 centers', {
        expect_equal(int_round(nrow(beer_cluster_k2$centers), 0), 2)
        })
    test_that('Solution is incorrect', {
        expect_equal(int_round(beer_cluster_k2$tot.withinss, 0), 1110)
        })
    print("Success!")
}

test_1.5 <- function(){
    test_that('tidy_beer_cluster_k2 should contain the columns: abv, ibu, and .cluster', {
    expect_true('abv' %in% colnames(tidy_beer_cluster_k2))
    expect_true('ibu' %in% colnames(tidy_beer_cluster_k2))
    expect_true('.cluster' %in% colnames(tidy_beer_cluster_k2))
    })
test_that('tidy_beer_cluster_k2 contains an incorrect number of rows and/or columns.', {
    expect_equal(int_round(nrow(tidy_beer_cluster_k2), 0), 1405)
    expect_equal(int_round(ncol(tidy_beer_cluster_k2), 0), 3)
    })
print("Success!")
    }

test_1.6 <- function(){
    properties <- c(tidy_beer_cluster_k2_plot$layers[[1]]$mapping, tidy_beer_cluster_k2_plot$mapping)
    labels <- tidy_beer_cluster_k2_plot$labels    
    test_that('Did not create a plot named tidy_beer_cluster_k2_plot', {
        expect_true(exists("tidy_beer_cluster_k2_plot")) 
        })
    test_that('tidy_beer_cluster_k2_plot should contain information from tidy_beer_cluster_k2', {
        expect_equal(tidy_beer_cluster_k2_plot$data, tidy_beer_cluster_k2)
        })
    test_that('ibu should be on the x-axis.', {
        expect_true("ibu" == rlang::get_expr(properties$x))
        })
    test_that('abv should be on the y-axis.', {
        expect_true("abv" == rlang::get_expr(properties$y))
        })
    test_that('.cluster should be used to colour the points.', {
        expect_true(".cluster" == rlang::get_expr(properties$colour))
        })
    test_that('tidy_beer_cluster_k2_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(tidy_beer_cluster_k2_plot$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false((labels$y) == 'abv')
        expect_false((labels$x) == 'ibu')
        expect_false((labels$colour) == '.cluster')
        })
    print("Success!")
    }

test_1.7.1 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(digest(answer1.7.1), '475bf9280aab63a82af60791302736f6')
})
print("Success!")
    }

test_1.7.2 <- function(){
    test_that('beer_cluster_k2_model_stats should be a tibble.', {
    expect_true('tbl' %in% class(beer_cluster_k2_model_stats))
    })
test_that('beer_cluster_k2_model_stats should have 1 row of 4 different statistics.', {
    expect_equal(dim(beer_cluster_k2_model_stats), c(1, 4))
    })
test_that('beer_cluster_k2_model_stats should contain total within sum of squares (tot.withinss).', {
    expect_true('tot.withinss' %in% colnames(beer_cluster_k2_model_stats))
    })
print("Success!")
    }

test_1.8 <- function(){
    test_that('beer_ks should be a tbl.', {
    expect_true('tbl' %in% class(beer_ks))
    })
test_that('beer_ks should have 1 column containing k values from 1 to 10.', {
    expect_equal(int_round(nrow(beer_ks), 0), 10) 
    expect_equal(int_round(ncol(beer_ks), 0), 1) 
    expect_equal(colnames(beer_ks), 'k') 
    })
print("Success!")
    }

test_1.9 <- function(){
    test_that('beer_clustering does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(beer_clustering), c(10, 2))
})
test_that('beer_clustering should contain the columns k and models', {
    expect_true('k' %in% colnames(beer_clustering)) 
    expect_true('models' %in% colnames(beer_clustering))
})
test_that('The models column in beer_clustering should be of class kmeans', {
    expect_equal(class(beer_clustering$models[[1]]), 'kmeans')
    })
print("Success!")
    }

test_2.0 <- function(){
    test_that('beer_model_stats does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(beer_model_stats), c(10, 3))
})
test_that('beer_model_stats should contain the columns k, models, and model_statistics', {
    expect_true('k' %in% colnames(beer_model_stats)) 
    expect_true('models' %in% colnames(beer_model_stats))
    expect_true('model_statistics' %in% colnames(beer_model_stats))
})
test_that('The models column in beer_model_stats should be of class kmeans', {
    expect_equal(class(beer_model_stats$models[[1]]), 'kmeans')
    })
test_that('The model_statistics column in beer_model_stats should be a tibble.', {
    expect_true('tbl' %in% class(beer_model_stats$model_statistics[[1]]))
    })
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(int_round(nrow(beer_clustering_unnested), 0), 10) 
    expect_equal(int_round(ncol(beer_clustering_unnested), 0), 6) 
    expect_true('k' %in% colnames(beer_clustering_unnested)) 
    expect_true('models' %in% colnames(beer_clustering_unnested))
    expect_false('model_statistics' %in% colnames(beer_clustering_unnested))
    expect_equal(class(beer_clustering_unnested$models[[1]]), 'kmeans')
    expect_true('tot.withinss' %in% colnames(beer_clustering_unnested))
})
print("Success!")
    }


test_2.2 <- function(){
    properties <- c(choose_beer_k$layers[[1]]$mapping, choose_beer_k$mapping)
    labels <- choose_beer_k$labels    
    test_that('Did not create a plot named choose_beer_k', {
        expect_true(exists("choose_beer_k")) 
        })
    test_that('# clusters should be on the x-axis.', {
        expect_true("k" == rlang::get_expr(properties$x))
        })
    test_that('total within-cluster sum-of-squares should be on the y-axis.', {
        expect_true("tot.withinss" == rlang::get_expr(properties$y))
        })
    test_that('choose_beer_k should be a line and scatter plot.', {
        expect_true("GeomLine" %in% c(class(choose_beer_k$layers[[1]]$geom),class(choose_beer_k$layers[[2]]$geom)))
        })
    test_that('choose_beer_k should be a line and scatter plot.', {
        expect_true("GeomPoint" %in% c(class(choose_beer_k$layers[[1]]$geom),class(choose_beer_k$layers[[2]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false((labels$y) == 'tot.withinss')
        expect_false((labels$x) == 'k')
        })
    print("Success!")
    }

test_2.3 <- function(){
    test_that('Solution is incorrect', {
    expect_true(digest(answer2.3) %in% c('0e4033b8c0b56afbea35dc749ced4e1d', 'd19d62a873f08af0488f0df720cfd293'))
    })
print("Success!")
    }

test_2.4 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(digest(answer2.4), '475bf9280aab63a82af60791302736f6')
    })
print("Success!")
    }

test_2.5 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(digest(answer2.5), '3a5505c06543876fe45598b5e5e5195d')
    })
print("Success!")
    }

test_2.6 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(digest(answer2.6), '05ca18b596514af73f6880309a21b5dd')
    })
print("Success!")
    }