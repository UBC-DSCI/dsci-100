library(testthat)
library(digest)

test_1.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.3.0 <- function(){
    test_that('Did not create an object named happy_df_csv', {
        expect_true(exists("happy_df_csv")) 
        })
    test_that('happy_df_csv should be a data frame.', {
        expect_true('data.frame' %in% class(happy_df_csv))
        })
    test_that('happy_df_csv does not contain the correct data.', {
        expect_equal(dim(happy_df_csv), c(1562, 19))
        expect_equal(digest(as.numeric(sum(happy_df_csv$year))), '17201cb95c3eb622e848455bd480ef23') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.3.1 <- function(){
    test_that('Did not create an object named happy_df', {
        expect_true(exists("happy_df")) 
        })
    test_that('happy_df should be a data frame.', {
        expect_true('data.frame' %in% class(happy_df))
        })
    test_that('happy_df does not contain the correct data.', {
        expect_equal(dim(happy_df), c(1562, 19))
        expect_equal(digest(as.numeric(sum(happy_df$year))), '17201cb95c3eb622e848455bd480ef23') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.3.2 <- function(){
    test_that('Did not create an object named happy_step1', {
        expect_true(exists("happy_step1")) 
        })
    test_that('Did not create an object named happy_step2', {
        expect_true(exists("happy_step2")) 
        })
    test_that('Did not create an object named reduced_happy_df', {
        expect_true(exists("reduced_happy_df")) 
        })
    test_that('reduced_happy_df does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(reduced_happy_df), c(141, 3))
        })
    test_that('The year column in reduced_happy_df should only contain 2017', {
        expect_equal(unique(reduced_happy_df$year), 2017)
        })
    test_that('Columns in reduced_happy_df contain incorrect values.', {
        expect_equal(digest(as.numeric(sum(reduced_happy_df$Positive.affect, na.rm = TRUE))), '3c52d24546a2dbf7437bfbf2b953614f')
        expect_equal(digest(as.numeric(sum(reduced_happy_df$Healthy.life.expectancy.at.birth, na.rm = TRUE))), 'dee33d6f7860c9997daacdbfded438cf') 
        })
print("Success!")
    }

test_1.4 <- function(){
    test_that('Did not create a plot named happy_plot', {
        expect_true(exists("happy_plot")) 
        })
    test_that('Healthy.life.expectancy.at.birth should be on the x-axis.', {
        expect_that("Healthy.life.expectancy.at.birth" %in% c(rlang::get_expr(happy_plot$mapping$x),rlang::get_expr(happy_plot$layers[[1]]$mapping$x)), is_true())
        })
    test_that('Positive.affect should be on the y-axis.', {
        expect_that("Positive.affect" %in% c(rlang::get_expr(happy_plot$mapping$y), rlang::get_expr(happy_plot$layers[[1]]$mapping$y)) , is_true())
        })
    test_that('happy_plot should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(happy_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((happy_plot$labels$y) == 'Positive.affect', is_false())
        expect_that((happy_plot$labels$x) == 'Healthy.life.expectancy.at.birth', is_false())
        })
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer2.1)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.2.0 <- function(){
    test_that('Did not create an object named whistler_2018', {
        expect_true(exists("whistler_2018")) 
        })
    test_that('whistler_2018 should be a data frame.', {
        expect_true('data.frame' %in% class(whistler_2018))
        })
    test_that('whistler_2018 does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(whistler_2018), c(365, 27))
        })
    test_that('whistler_2018 does not contain the correct data.', {
        expect_equal(digest(as.numeric(sum(whistler_2018$Snow_ground, na.rm = TRUE))), '908d1fd10b357ed0ceaaec823abf81bc') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.2.1 <- function(){
    test_that('whistler_2018 does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(whistler_2018), c(365, 27))
        })
    test_that('whistler_2018 still contains white space in its column names.', {
        expect_equal(length(grep(pattern = " ", x = colnames(whistler_2018))), 0)    
        })
print("Success!")
    }

test_2.3 <- function(){
    test_that('Did not create a plot named whistler_2018_plot', {
        expect_true(exists("whistler_2018_plot")) 
        })
    test_that('Date.Time should be on the x-axis.', {
        expect_that("Date.Time" %in% c(rlang::get_expr(whistler_2018_plot$mapping$x),rlang::get_expr(whistler_2018_plot$layers[[1]]$mapping$x)), is_true())
        })
    test_that('Total.Snow..cm. should be on the y-axis.', {
        expect_that("Total.Snow..cm." %in% c(rlang::get_expr(whistler_2018_plot$mapping$y), rlang::get_expr(whistler_2018_plot$layers[[1]]$mapping$y)) , is_true())
        })
    test_that('whistler_2018_plot should be a line plot.', {
        expect_that("GeomLine" %in% c(class(whistler_2018_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((whistler_2018_plot$labels$y) == 'Total.Snow..cm.', is_false())
        expect_that((whistler_2018_plot$labels$x) == 'Date.Time', is_false())
        })
print("Success!")
    }

test_3.0 <- function(){
    test_that('Did not create a database connected called connection', {
        expect_true(exists("connection"))
        expect_equal(digest(class(connection)), 'df66af2493a2405d1c7763f4bfbfebc0')
        })
    test_that('Did not create a table called project_data', {
        expect_true(exists('project_data'))
        expect_equal(digest(class(project_data))[[1]], '98f4e628b20def80372d0acca8b5edb0')
    })
    test_that('The project_data table has the wrong information in it', {
        expect_equal(digest(colnames(project_data)[[13]]), 'fd0b87dfaa67d4c90844080d5d9355ec')
        expect_equal(digest(colnames(project_data)[[14]]), '2f2ec36ffb7792b1f8e274387e520edd')
        expect_equal(digest(length(colnames(project_data))), '4d4c1ad2286f1a7670a024467dd10808')
        expect_equal(digest(as_tibble(summarize(project_data, sum=sum(goal, na.rm=TRUE)))[[1]]), 'f8d1fabeacced2ef4c82748ca212b724')
    })
print('Success!')
    }

test_3.1 <- function(){
    test_that('Incorrect answer', {
        expect_equal(digest(answer3.1), '3a5505c06543876fe45598b5e5e5195d')
    })
print('Success!')
}


test_3.2 <- function(){
    test_that('Did not create a plot named funding_over_time_plot', {
        expect_true(exists("funding_over_time_plot"))
        })
    test_that('The axis labels are incorrect', {
        expect_equal(digest(funding_over_time_plot$labels$x), "bd1055a7d6af03e74a4462edbc5b35c4")
        expect_equal(digest(funding_over_time_plot$labels$y), '48c36ecb38a110dffe3f40add37ccdc1')
        })
    test_that('The x/y data are incorrect', {
        expect_equal(digest(funding_over_time_plot$mapping$x), 'c5a397ebe58129a88408811144869370')
        expect_equal(digest(funding_over_time_plot$mapping$y), '5d75d853465e03f6828b25a1808b073b')
        })
    test_that('arrival_delay_plot should be a scatter plot', {
        expect_equal(digest(class(funding_over_time_plot$layers[[1]]$geom)[[1]]), '911e5b9debfb523f25ad2ccc01a4b2dd')
     })
print("Success!")
}

test_3.4 <- function(){
    test_that('Did not create project_data.csv in the data/ folder.', {
        expect_true(file.exists("data/project_data.csv"))
        })
    test_that('the project_data.csv file does not contain the right data', {
        expect_equal(digest(sum(read_csv('data/project_data.csv'))), "daca6d59f60da17a8beacb0b8caa51b9")
        })
    test_that('the project_df object does not exist', {
        expect_true(exists('project_df'))
        })
    test_that('the project_df object does not have the right number of rows & columns',{
        expect_equal(digest(nrow(project_df)), '40202635f105eddd66654976af5d9e1f')
        expect_equal(digest(ncol(project_df)), '11946e7a3ed5e1776e81c0f0ecd383d0')
        })
    test_that('the project_df object does not have the right data',{
        expect_equal(digest(sum(project_df)), 'daca6d59f60da17a8beacb0b8caa51b9')
        })
print("Success!")
}








