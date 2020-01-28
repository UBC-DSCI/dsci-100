library(testthat)
library(digest)

test_0.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.1_A), '05ca18b596514af73f6880309a21b5dd') 
        expect_equal(digest(answer0.1_B), 'd2a90307aac5ae8d0ef58e2fe730d38b')
        expect_equal(digest(answer0.1_C), '05ca18b596514af73f6880309a21b5dd')
        expect_equal(digest(answer0.1_D), '05ca18b596514af73f6880309a21b5dd')
        expect_equal(digest(answer0.1_E), 'd2a90307aac5ae8d0ef58e2fe730d38b')# we hid the answer to the test here so you can't see it, but we can still run the test 
        })
print("Success!")
    }
    
test_0.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.2), '75f1160e72554f4270c809f041c7a776') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }
    
test_0.3 <- function(){   
   test_that('Solution is incorrect', {
       expect_equal(digest(answer0.3), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test    
        })
print("Success!")
    }
    
test_0.4 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.4), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_0.5 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer0.5_1), '75f1160e72554f4270c809f041c7a776') 
        expect_equal(digest(answer0.5_2), '01a75cb73d67b0f895ff0e61449c7bf8')
        expect_equal(digest(answer0.5_3), '3a5505c06543876fe45598b5e5e5195d')
        expect_equal(digest(answer0.5_4), 'f76b651ab8fcb8d470f79550bf2af53a')
        expect_equal(digest(answer0.5_5), 'c1f86f7430df7ddb256980ea6a3b57a4')
        expect_equal(digest(answer0.5_6), '475bf9280aab63a82af60791302736f6')# we hid the answer to the test here so you can't see it, but we can still run the test 
        })
print("Success!")
    }

test_1.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.0), '75f1160e72554f4270c809f041c7a776') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.1 <- function(){
    test_that('Did not create an object named world_vaccination', {
        expect_true(exists("world_vaccination")) 
        })
    test_that('world_vaccination should be a data frame.', {
        expect_true('data.frame' %in% class(world_vaccination))
        })
    test_that('Did not remove NA values from the pct_vaccinated column.', {
        expect_equal(any(is.na(world_vaccination$pct_vaccinated)), FALSE)
        })
    test_that('world_vaccination does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(world_vaccination), c(385, 4))
        })
    test_that('Columns in world_vaccination contain incorrect values.', {
        expect_equal(digest(as.numeric(sum(world_vaccination$yr))), 'ed828cd9c4dbc736fb12a1a8643aaeec')
        })
print("Success!")
    }

test_1.2 <- function(){
    test_that('Did not create a plot named world_vacc_plot', {
        expect_true(exists("world_vacc_plot")) 
        })
    test_that('year should be on the x-axis.', {
        expect_that("yr" %in% c(rlang::get_expr(world_vacc_plot$mapping$x),rlang::get_expr(world_vacc_plot$layers[[1]]$mapping$x)), is_true())
        })
    test_that('pct_vaccinated should be on the y-axis.', {
        expect_that("pct_vaccinated" %in% c(rlang::get_expr(world_vacc_plot$mapping$y), rlang::get_expr(world_vacc_plot$layers[[1]]$mapping$y)) , is_true())
        })
    test_that('world_vacc_plot should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(world_vacc_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((world_vacc_plot$labels$y) == 'pct_vaccinated', is_false())
        expect_that((world_vacc_plot$labels$x) == 'yr', is_false())
        })
print("Success!")
    }

test_1.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.3), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.4 <- function(){
    properties <- c(compare_vacc_plot$layers[[1]]$mapping, compare_vacc_plot$mapping)
    labels <- compare_vacc_plot$labels
    test_that('Did not create a plot named compare_vacc_plot', {
        expect_true(exists("compare_vacc_plot")) 
        })
    test_that('year should be on the x-axis.', {
        expect_true("yr" == rlang::get_expr(properties$x))
        })
    test_that('pct_vaccinated should be on the y-axis.', {
        expect_true("pct_vaccinated" == rlang::get_expr(properties$y))
        })
    test_that('vaccine should map to colour and shape.', {
        expect_true("vaccine" == rlang::get_expr(properties$colour))
        })
    test_that('vaccine should map to shape and colour.', {
        expect_true("vaccine" == rlang::get_expr(properties$shape))
        })
        test_that('vaccine should map to shape and colour.', {
        expect_that("character" %in% class(compare_vacc_plot$layers[[1]]$mapping$shape), is_false())
        })
        test_that('vaccine should map to shape and colour.', {
        expect_that("character" %in% class(compare_vacc_plot$layers[[1]]$mapping$colour), is_false())
        })
    test_that('compare_vacc_plot should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(compare_vacc_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes and legend need to be changed to be descriptive, nicely formatted, and human readable.', {
        expect_that((labels$y) == 'pct_vaccinated', is_false())
        expect_that((labels$x) == 'yr', is_false())
        expect_that((labels$colour) == 'vaccine', is_false())
        expect_that((labels$shape) == 'vaccine', is_false())
        })
print("Success!")
    }

test_1.5 <- function(){
    test_that('Did not create an object named polio', {
        expect_true(exists("polio")) 
        })
    test_that('The vaccine column in polio should only contain the polio vaccine.', {
        expect_equal(unique(polio$vaccine), "polio")
        })
    test_that('polio does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(polio), c(228, 4))
        })
    test_that('Columns in polio contain incorrect values.', {
        expect_equal(digest(as.numeric(sum(polio$pct_vaccinated))), 'cfd7ed9e50ed446d50289ff89ef338a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.6 <- function(){
    properties <- c(polio_regions$layers[[1]]$mapping, polio_regions$mapping)
    labels <- polio_regions$labels
    test_that('Did not create a plot named polio_regions', {
        expect_true(exists("polio_regions")) 
        })
    test_that('year should be on the x-axis.', {
        expect_true("yr" == rlang::get_expr(properties$x))
        })
    test_that('pct_vaccinated should be on the y-axis.', {
        expect_true("pct_vaccinated" == rlang::get_expr(properties$y))
        })
    test_that('who_region should map to colour and shape.', {
        expect_true("who_region" == rlang::get_expr(properties$colour))
        })
    test_that('who_region should map to shape and colour.', {
        expect_true("who_region" == rlang::get_expr(properties$shape))
        })
    test_that('polio_regions should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(polio_regions$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'pct_vaccinated', is_false())
        expect_that((labels$x) == 'yr', is_false())
        })
print("Success!")
    }


test_1.7.1 <- function(){
    
    properties <- c(polio_regions_line$layers[[1]]$mapping, polio_regions_line$mapping)
    labels <- polio_regions_line$labels
    
    test_that('Did not create a plot named polio_regions_line', {
        expect_true(exists("polio_regions_line")) 
        })
    test_that('year should be on the x-axis.', {
        expect_true("yr" == rlang::get_expr(properties$x))
        })
    test_that('pct_vaccinated should be on the y-axis.', {
        expect_true("pct_vaccinated" == rlang::get_expr(properties$y))
        })
    test_that('who_region should map to colour.', {
        expect_true("who_region" == rlang::get_expr(properties$colour))
        })
    
    if (length(polio_regions_line$layers) == 2) {  
    test_that('who_region should map to colour.', {
        expect_that("who_region" == rlang::get_expr(c(polio_regions_line$layers[[1]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(polio_regions_line$layers[[2]]$mapping)$colour) | 
                    "who_region" == rlang::get_expr(c(polio_regions_line$layers[[2]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(polio_regions_line$layers[[1]]$mapping)$colour), is_true()) 
                    } )}  else {
        test_that('who_region should map to colour.', {
        expect_true("who_region" == rlang::get_expr(properties$colour))
        })
        }    
    
    if (length(polio_regions_line$layers) == 2) { # if there are 2 layers then check if the two layers are points and lines 
    test_that('polio_regions_line should be a line plot.', {
        expect_that("GeomPoint" %in% c(class(polio_regions_line$layers[[1]]$geom)) & 
                    "GeomLine" %in% c(class(polio_regions_line$layers[[2]]$geom)) | 
                    "GeomPoint" %in% c(class(polio_regions_line$layers[[2]]$geom)) & 
                    "GeomLine" %in% c(class(polio_regions_line$layers[[1]]$geom)), is_true()) 
                    } )}  else {
    test_that('polio_regions_line should be a line plot.', { 
        expect_that("GeomLine" %in% c(class(polio_regions_line$layers[[1]]$geom)) , is_true())
        })
        }    
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((labels$y) == 'pct_vaccinated', is_false())
        expect_that((labels$x) == 'yr', is_false())
        })
print("Success!")
    }


test_1.7.2 <- function(){
    properties <- c(polio_regions_line$layers[[1]]$mapping, polio_regions_line$mapping)
    labels <- polio_regions_line$labels
    test_that('Did not create a plot named polio_regions_line', {
        expect_true(exists("polio_regions_line")) 
        })
    test_that('year should be on the x-axis.', {
        expect_true("yr" == rlang::get_expr(properties$x))
        })
    test_that('pct_vaccinated should be on the y-axis.', {
        expect_true("pct_vaccinated" == rlang::get_expr(properties$y))
        })
    test_that('who_region should map to colour.', {
        expect_true("who_region" == rlang::get_expr(properties$colour))
        })
   
    if (length(polio_regions_line$layers) == 2) {  
    test_that('who_region should map to colour.', {
        expect_that("who_region" == rlang::get_expr(c(polio_regions_line$layers[[1]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(polio_regions_line$layers[[2]]$mapping)$colour) | 
                    "who_region" == rlang::get_expr(c(polio_regions_line$layers[[2]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(polio_regions_line$layers[[1]]$mapping)$colour), is_true()) 
                    } )}  else {
        test_that('who_region should map to colour.', {
        expect_true("who_region" == rlang::get_expr(properties$colour))
        })
        }    
    
    if (length(polio_regions_line$layers) == 2) { # if there are 2 layers then check if the two layers are points and lines 
    test_that('polio_regions_line should be a line plot.', {
        expect_that("GeomPoint" %in% c(class(polio_regions_line$layers[[1]]$geom)) & 
                    "GeomLine" %in% c(class(polio_regions_line$layers[[2]]$geom)) | 
                    "GeomPoint" %in% c(class(polio_regions_line$layers[[2]]$geom)) & 
                    "GeomLine" %in% c(class(polio_regions_line$layers[[1]]$geom)), is_true()) 
                    } )}  else {
    test_that('polio_regions_line should be a line plot.', { 
        expect_that("GeomLine" %in% c(class(polio_regions_line$layers[[1]]$geom)) , is_true())
        })
        }     
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'pct_vaccinated', is_false())
        expect_that((labels$x) == 'yr', is_false())
        expect_that((labels$colour) == 'who_region', is_false())
        })
print("Success!")
    }


test_1.8 <- function(){
    properties <- c(side_by_side_world$layers[[1]]$mapping, side_by_side_world$mapping)
    labels <- side_by_side_world$labels
    test_that('Did not create a plot named side_by_side_world', {
        expect_true(exists("side_by_side_world")) 
        })
    test_that('year should be on the x-axis.', {
        expect_true("yr" == rlang::get_expr(properties$x))
        })
    test_that('pct_vaccinated should be on the y-axis.', {
        expect_true("pct_vaccinated" == rlang::get_expr(properties$y))
        })
    test_that('side_by_side_world should be faceted by the vaccine column.', {
        expect_that('FacetGrid' %in% class(rlang::get_expr(side_by_side_world$facet)), is_true())
        })
    test_that("side_by_side_world should be split horizontally", {
        expect_that("vaccine" %in% names(rlang::get_expr(side_by_side_world$facet$params$cols)), is_true())
    })
       if (length(side_by_side_world$layers) == 2) {  
    test_that('who_region should map to colour.', {
        expect_that("who_region" == rlang::get_expr(c(side_by_side_world$layers[[1]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(side_by_side_world$layers[[2]]$mapping)$colour) | 
                    "who_region" == rlang::get_expr(c(side_by_side_world$layers[[2]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(side_by_side_world$layers[[1]]$mapping)$colour), is_true()) 
                    } )}  else {
        test_that('who_region should map to colour.', {
        expect_true("who_region" == rlang::get_expr(properties$colour))
        })
        }    
    
    if (length(side_by_side_world$layers) == 2) { # if there are 2 layers then check if the two layers are points and lines 
    test_that('side_by_side_world should be a line plot.', {
        expect_that("GeomPoint" %in% c(class(side_by_side_world$layers[[1]]$geom)) & 
                    "GeomLine" %in% c(class(side_by_side_world$layers[[2]]$geom)) | 
                    "GeomPoint" %in% c(class(side_by_side_world$layers[[2]]$geom)) & 
                    "GeomLine" %in% c(class(side_by_side_world$layers[[1]]$geom)), is_true()) 
                    } )}  else {
    test_that('side_by_side_world should be a line plot.', { 
        expect_that("GeomLine" %in% c(class(side_by_side_world$layers[[1]]$geom)) , is_true())
        })
        }     
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'pct_vaccinated', is_false())
        expect_that((labels$x) == 'yr', is_false())
        expect_that((labels$colour) == 'who_region', is_false())
        })
print("Success!")
    }

test_1.9.1 <- function(){
    properties <- c(vertical_world$layers[[1]]$mapping, vertical_world$mapping)
    labels <- vertical_world$labels
    test_that('Did not create a plot named vertical_world', {
        expect_true(exists("vertical_world")) 
        })
    test_that('year should be on the x-axis.', {
        expect_true("yr" == rlang::get_expr(properties$x))
        })
    test_that('pct_vaccinated should be on the y-axis.', {
        expect_true("pct_vaccinated" == rlang::get_expr(properties$y))
        })
    test_that('vertical_world should be faceted by the vaccine column.', {
        expect_that('FacetGrid' %in% class(rlang::get_expr(vertical_world$facet)), is_true())
        })
      test_that("vertical_world should be split vertically", {
        expect_that("vaccine" %in% names(rlang::get_expr(vertical_world$facet$params$rows)), is_true())
    })
 if (length(vertical_world$layers) == 2) {  
    test_that('who_region should map to colour.', {
        expect_that("who_region" == rlang::get_expr(c(vertical_world$layers[[1]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(vertical_world$layers[[2]]$mapping)$colour) | 
                    "who_region" == rlang::get_expr(c(vertical_world$layers[[2]]$mapping)$colour) & 
                    "who_region" == rlang::get_expr(c(vertical_world$layers[[1]]$mapping)$colour), is_true()) 
                    } )}  else {
        test_that('who_region should map to colour.', {
        expect_true("who_region" == rlang::get_expr(properties$colour))
        })
        }    
    
    if (length(vertical_world$layers) == 2) { # if there are 2 layers then check if the two layers are points and lines 
    test_that('vertical_world should be a line plot.', {
        expect_that("GeomPoint" %in% c(class(vertical_world$layers[[1]]$geom)) & 
                    "GeomLine" %in% c(class(vertical_world$layers[[2]]$geom)) | 
                    "GeomPoint" %in% c(class(vertical_world$layers[[2]]$geom)) & 
                    "GeomLine" %in% c(class(vertical_world$layers[[1]]$geom)), is_true()) 
                    } )}  else {
    test_that('vertical_world should be a line plot.', { 
        expect_that("GeomLine" %in% c(class(vertical_world$layers[[1]]$geom)) , is_true())
        })
        }
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'pct_vaccinated', is_false())
        expect_that((labels$x) == 'yr', is_false())
        expect_that((labels$colour) == 'who_region', is_false())
        })
print("Success!")
    }

test_1.9.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.9.2), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.1), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.2 <- function(){
    test_that('Did not create an object named fast_food', {
        expect_true(exists("fast_food")) 
        })
    test_that('fast_food does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(fast_food), c(10000, 2))
        })
    test_that('Columns in fast_food contain incorrect values.', {
        expect_equal(digest(as.character(fast_food[[3,1]])), '2e716500dfeb89b1b087089a5b1355f8') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(as.character(fast_food[[4,2]])), 'd599245d7d7e3f56863ba3a6112ca71b') 
        })
print("Success!")
    }

test_2.3 <- function(){
    test_that('Did not create an object named top_restaurants', {
        expect_true(exists("top_restaurants")) 
        })
    test_that('top_restaurants does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(top_restaurants), c(9, 2))
        })
    test_that('Columns in fast_food contain incorrect values.', {
        expect_equal(digest(sum(as.numeric(top_restaurants$n, na.rm = TRUE))), '4745bd90664c8cb9935cfb1a4cf51d77') 
        })
print("Success!")
    }

test_2.4 <- function(){
    properties <- c(count_bar_chart$layers[[1]]$mapping, count_bar_chart$mapping)
    labels <- count_bar_chart$labels
    test_that('Did not create a plot named count_bar_chart', {
        expect_true(exists("count_bar_chart")) 
        })
    test_that('name should be on the x-axis.', {
        expect_true("name" == rlang::get_expr(properties$x))
        })
    test_that('n should be on the y-axis.', {
        expect_true("n" == rlang::get_expr(properties$y))
        })
    test_that('vertical_world should be a bar plot.', {
        expect_that("GeomBar" %in% c(class(count_bar_chart$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'n', is_false())
        expect_that((labels$x) == 'name', is_false())
        })
print("Success!")
    }

test_2.5_A <- function(){
    properties <- c(count_bar_chart_A$layers[[1]]$mapping, count_bar_chart_A$mapping)
    labels <- count_bar_chart_A$labels
    test_that('Did not create a plot named count_bar_chart_A', {
        expect_true(exists("count_bar_chart_A")) 
        })
    test_that('name should be on the x-axis.', {
        expect_true("name" == rlang::get_expr(properties$x))
        })
    test_that('x-axis (bar) labels should be at an angle between 20 and 90 degrees.', {
        expect_that(count_bar_chart_A$theme$axis.text.x$angle <= 90 & count_bar_chart_A$theme$axis.text.x$angle >= 20, is_true())
        })
    test_that('hjust should equal 1', {
        expect_equal(digest(as.numeric(count_bar_chart_A$theme$axis.text.x$hjust)), '6717f2823d3202449301145073ab8719')
        })
    test_that('n should be on the y-axis.', {
        expect_true("n" == rlang::get_expr(properties$y))
        })
    test_that('vertical_world should be a bar plot.', {
        expect_that("GeomBar" %in% c(class(count_bar_chart_A$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'n', is_false())
        expect_that((labels$x) == 'name', is_false())
        })
print("Success!")
    }

test_2.5_B <- function(){
    properties <- c(count_bar_chart_B$layers[[1]]$mapping, count_bar_chart_B$mapping)
    labels <- count_bar_chart_B$labels
    test_that('Did not create a plot named count_bar_chart_B', {
        expect_true(exists("count_bar_chart_B")) 
        })
    test_that('name should be on the x-axis.', {
        expect_true("name" == rlang::get_expr(properties$x))
        })
    test_that('The coordinate axes should be flipped', {
        expect_true("CoordFlip" %in% class(ggplot_build(count_bar_chart_B)$layout$coord))
        })
    test_that('n should be on the y-axis.', {
        expect_true("n" == rlang::get_expr(properties$y))
        })
    test_that('vertical_world should be a bar plot.', {
        expect_that("GeomBar" %in% c(class(count_bar_chart_B$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'n', is_false())
        expect_that((labels$x) == 'name', is_false())
        })
print("Success!")
    }

test_2.6 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.6), '948a9b527842ee791d4f18fb5594fbf7')
        })
print("Success!")
    }

test_2.7 <- function(){
    test_that('Did not create an object named state_counts', {
        expect_true(exists("state_counts")) 
        })
    test_that('The state column in state_counts should only contain CA, OR, and WA', {
        expect_equal((state_counts$st), c("CA", "OR", "WA"))
        })
    test_that('state_counts does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(state_counts), c(3, 2))
        })
    test_that('Columns in state_counts contain incorrect values.', {
        expect_equal(digest(sum(as.numeric(state_counts$n, na.rm = TRUE))), '4745bd90664c8cb9935cfb1a4cf51d77') 
        })
print("Success!")
    }

test_2.8 <- function(){
    properties <- c(state_counts_plot$layers[[1]]$mapping, state_counts_plot$mapping)
    labels <- state_counts_plot$labels
    test_that('Did not create a plot named state_counts_plot', {
        expect_true(exists("state_counts_plot")) 
        })
    test_that('state should be on the x-axis.', {
        expect_true("st" == rlang::get_expr(properties$x))
        })
    test_that('n should be on the y-axis.', {
        expect_true("n" == rlang::get_expr(properties$y))
        })
    test_that('state_counts_plot should be a bar plot.', {
        expect_that("GeomBar" %in% c(class(state_counts_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('state_counts_plot should not be filled.', {
        expect_that("PositionFill" %in% class(state_counts_plot$layers[[1]]$position), is_false()) 
        })
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'n', is_false())
        expect_that((labels$x) == 'st', is_false())
        })
print("Success!")
    }

test_2.9.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.9.1), '2bedd54d48692762c119b27f5ec7a320')
        })
print("Success!")
    }

test_2.9.2 <- function(){
    test_that('Did not create an object named top_n_state', {
        expect_true(exists("top_n_state")) 
        })
    test_that('The state column in top_n_state should only contain CA, OR, and WA', {
        expect_equal(digest(top_n_state$st), '8e6ddcfd9ebaf85379a6d46f7949bce0')
        })
    test_that('top_n_state does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(top_n_state), c(27, 3))
        })
    test_that('Columns in top_n_state contain incorrect values.', {
        expect_equal(digest(sum(as.numeric(top_n_state$n, na.rm = TRUE))), '4745bd90664c8cb9935cfb1a4cf51d77') 
        })
print("Success!")
    }

test_2.9.3 <- function(){
    properties <- c(top_n_state_plot$layers[[1]]$mapping, top_n_state_plot$mapping)
    labels <- top_n_state_plot$labels
    test_that('Did not create a plot named top_n_state_plot', {
        expect_true(exists("top_n_state_plot")) 
        })
    test_that('state should be on the x-axis.', {
        expect_true("st" == rlang::get_expr(properties$x))
        })
    test_that('n should be on the y-axis.', {
        expect_true("n" == rlang::get_expr(properties$y))
        })
    test_that('name should be used to determine bar fill colour.', {
        expect_true("name" == rlang::get_expr(properties$fill)) 
        })
    test_that('top_n_state_plot should be a bar plot.', {
        expect_that("GeomBar" %in% c(class(top_n_state_plot$layers[[1]]$geom)) , is_true())
        })
       test_that('top_n_state_plot position should be dodge.', {
        expect_that("PositionDodge" %in% class(top_n_state_plot$layers[[1]]$position) , is_true())
        })
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'n', is_false())
        expect_that((labels$x) == 'st', is_false())
        expect_that((labels$fill) == 'name', is_false())
        })
print("Success!")
    }

test_2.9.4 <- function(){
    properties <- c(top_n_state_plot$layers[[1]]$mapping, top_n_state_plot$mapping)
    labels <- top_n_state_plot$labels
    test_that('Did not create a plot named top_n_state_plot', {
        expect_true(exists("top_n_state_plot")) 
        })
    test_that('state should be on the x-axis.', {
        expect_true("st" == rlang::get_expr(properties$x))
        })
    test_that('n should be on the y-axis.', {
        expect_true("n" == rlang::get_expr(properties$y))
        })
    test_that('name should be used to determine bar fill colour.', {
        expect_true("name" == rlang::get_expr(properties$fill)) 
        })
    test_that('top_n_state_plot should be a bar plot.', {
        expect_that("GeomBar" %in% c(class(top_n_state_plot$layers[[1]]$geom)) , is_true())
        })
     test_that('top_n_state_plot position should be fill', {
        expect_that("PositionFill" %in% class(top_n_state_plot$layers[[1]]$position) , is_true())
        })
    test_that('Labels on the axes and legend should be descriptive and human readable.', {
        expect_that((labels$y) == 'n', is_false())
        expect_that((labels$x) == 'st', is_false())
        expect_that((labels$x) == 'name', is_false())
        })
print("Success!")
    }

test_2.9.5 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.9.5), '0590b0427c1b19a6eb612d19888aa52f') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }