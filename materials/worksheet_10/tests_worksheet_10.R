library(testthat)
library(digest)

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
    test_that('beer should be a data frame.', {
        expect_true('data.frame' %in% class(beer))
    })
    test_that('beer does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(beer), c(2410, 8))
    })
    test_that('The beer data frame is missing columns.', {
        expect_that("X1" %in% colnames(beer), is_true())
        expect_that("abv" %in% colnames(beer), is_true())
        expect_that("ibu" %in% colnames(beer), is_true())
        expect_that("id" %in% colnames(beer), is_true())
        expect_that("name" %in% colnames(beer), is_true())
        expect_that("style" %in% colnames(beer), is_true())
        expect_that("brewery_id" %in% colnames(beer), is_true())
        expect_that("ounces" %in% colnames(beer), is_true())
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
        expect_that("GeomPoint" %in% c(class(beer_eda$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((labels$y) == 'abv', is_false())
        expect_that((labels$x) == 'ibu', is_false())
        })
    print("Success!")
}

test_1.2 <- function(){
    test_that('Did not create an object named clean_beer', {
        expect_true(exists("clean_beer")) 
    })
    test_that('clean_beer should be a data frame.', {
        expect_true('data.frame' %in% class(clean_beer))
    })
    test_that('clean_beer does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(clean_beer), c(1405, 2))
    })
    test_that('clean_beer should only contain the columns ibu and abv', {
        expect_that("ibu" %in% colnames(clean_beer), is_true())
        expect_that("abv" %in% colnames(clean_beer), is_true())
        expect_that("id" %in% colnames(clean_beer), is_false())
        expect_that("name" %in% colnames(clean_beer), is_false())
        expect_that("style" %in% colnames(clean_beer), is_false())
        expect_that("brewery_id" %in% colnames(clean_beer), is_false())
        expect_that("ounces" %in% colnames(clean_beer), is_false())
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
    test_that('scaled_beer should be a data frame.', {
        expect_true('data.frame' %in% class(scaled_beer))
    })
    test_that('scaled_beer does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(scaled_beer), c(1405, 2))
    })
    test_that('scaled_beer should only contain the columns ibu and abv', {
        expect_that("ibu" %in% colnames(clean_beer), is_true())
        expect_that("abv" %in% colnames(clean_beer), is_true())
        expect_that("id" %in% colnames(clean_beer), is_false())
        expect_that("name" %in% colnames(clean_beer), is_false())
        expect_that("style" %in% colnames(clean_beer), is_false())
        expect_that("brewery_id" %in% colnames(clean_beer), is_false())
        expect_that("ounces" %in% colnames(clean_beer), is_false())
        })
    test_that('Columns in scaled_beer are not scaled correctly.', {
        expect_that(min(scaled_beer$ibu) < 1, is_true())
        expect_that(max(scaled_beer$ibu) < 4, is_true())
        expect_that(min(scaled_beer$abv) < -2, is_true())
        expect_that(max(scaled_beer$abv) < 5, is_true())
        })
    print("Success!")
}

test_1.4 <- function(){
    test_that('beer_cluster_k2 class should be kmeans', {
        expect_equal(class(beer_cluster_k2), 'kmeans')
        })
    test_that('beer_cluster_k2 should have 2 centers', {
        expect_equal(nrow(beer_cluster_k2$centers), 2)
        })
    test_that('Solution is incorrect', {
        expect_equal(round(beer_cluster_k2$tot.withinss), 1110)
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
    expect_equal(nrow(tidy_beer_cluster_k2), 1405)
    expect_equal(ncol(tidy_beer_cluster_k2), 3)
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
        expect_that("GeomPoint" %in% c(class(tidy_beer_cluster_k2_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((labels$y) == 'abv', is_false())
        expect_that((labels$x) == 'ibu', is_false())
        expect_that((labels$colour) == '.cluster', is_false())
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
    test_that('beer_cluster_k2_model_stats should be a data frame.', {
    expect_true('data.frame' %in% class(beer_cluster_k2_model_stats))
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
    test_that('beer_clustering should be a data frame.', {
    expect_true('data.frame' %in% class(beer_clustering))
    })
test_that('beer_clustering should have 1 column containing k values from 1 to 10.', {
    expect_equal(nrow(beer_clustering), 10) 
    expect_equal(ncol(beer_clustering), 1) 
    expect_equal(colnames(beer_clustering), 'k') 
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
    test_that('beer_clustering does not contain the correct number of rows and/or columns.', {
    expect_equal(dim(beer_clustering), c(10, 3))
})
test_that('beer_clustering should contain the columns k, models, and model_statistics', {
    expect_true('k' %in% colnames(beer_clustering)) 
    expect_true('models' %in% colnames(beer_clustering))
    expect_true('model_statistics' %in% colnames(beer_clustering))
})
test_that('The models column in beer_clustering should be of class kmeans', {
    expect_equal(class(beer_clustering$models[[1]]), 'kmeans')
    })
test_that('The model_statistics column in beer_clustering should be a data frame.', {
    expect_true('data.frame' %in% class(beer_clustering$model_statistics[[1]]))
    })
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
    expect_equal(nrow(beer_clustering_unnested), 10) 
    expect_equal(ncol(beer_clustering_unnested), 6) 
    expect_true('k' %in% colnames(beer_clustering_unnested)) 
    expect_true('models' %in% colnames(beer_clustering_unnested))
    expect_false('model_statistics' %in% colnames(beer_clustering_unnested))
    expect_equal(class(beer_clustering_unnested$models[[1]]), 'kmeans')
    expect_true('tot.withinss' %in% colnames(beer_clustering_unnested))
})
print("Success!")
    }

test_2.2 <- function(){
    test_that('Did not create a plot named choose_beer_k', {
    expect_true(exists("choose_beer_k")) 
    })
test_that('choose_beer_k contains incorrect information.', {
    expect_equal(digest(choose_beer_k$data$k), 'c08951d2c283a799ab013bf845ed822e')
    expect_equal(digest(choose_beer_k$data$tot.withinss), '11d131cf0b717bfbe3acbed4eacad2c6')
    })
test_that('k should be on the x-axis.', {
    expect_that("k" %in% c(rlang::get_expr(choose_beer_k $mapping$x),rlang::get_expr(choose_beer_k $layers[[1]]$mapping$x)), is_true())
    })
test_that('tot.withinss should be on the y-axis.', {
    expect_true(as.character(rlang::get_expr(choose_beer_k$mapping$y)) %in% c("tot.withinss"))
    })
test_that('choose_beer_k should be a scatter plot and a line plot.', {
    expect_that('GeomPoint' %in% c(class(rlang::get_expr(choose_beer_k $layers[[1]]$geom)), class(rlang::get_expr(choose_beer_k $layers[[2]]$geom))), is_true())
    expect_that('GeomLine' %in% c(class(rlang::get_expr(choose_beer_k $layers[[1]]$geom)), class(rlang::get_expr(choose_beer_k $layers[[2]]$geom))), is_true())
    })
print("Success!")
    }


test_2.2 <- function(){
    properties <- c(choose_beer_k$layers[[1]]$mapping, choose_beer_k$mapping)
    labels <- choose_beer_k$labels    
    test_that('Did not create a plot named choose_beer_k', {
        expect_true(exists("choose_beer_k")) 
        })
    test_that('choose_beer_k should contain information from beer_clustering_unnested', {
        expect_equal(digest(choose_beer_k$data$k), 'c08951d2c283a799ab013bf845ed822e')
        expect_equal(digest(choose_beer_k$data$tot.withinss), '11d131cf0b717bfbe3acbed4eacad2c6')
        })
    test_that('# clusters should be on the x-axis.', {
        expect_true("k" == rlang::get_expr(properties$x))
        })
    test_that('total within-cluster sum-of-squares should be on the y-axis.', {
        expect_true("tot.withinss" == rlang::get_expr(properties$y))
        })
    test_that('choose_beer_k should be a scatter plot.', {
        expect_that("GeomLine" %in% c(class(choose_beer_k$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((labels$y) == 'tot.withinss', is_false())
        expect_that((labels$x) == 'k', is_false())
        })
    print("Success!")
    }

test_2.3 <- function(){
    test_that('Solution is incorrect', {
    expect_true(digest(answer2.3) %in% c('0e4033b8c0b56afbea35dc749ced4e1d', 'd19d62a873f08af0488f0df720cfd293'))
    })
print("Success!")
    }
