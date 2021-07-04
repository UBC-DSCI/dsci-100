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

test_1.1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1.1), "475bf9280aab63a82af60791302736f6")
  })
  print("Success!")
}

test_1.2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1.2), "c1f86f7430df7ddb256980ea6a3b57a4")
  })
  print("Success!")
}

test_1.3.0 <- function() {
  test_that("Did not create an object named happy_df_csv", {
    expect_true(exists("happy_df_csv"))
  })
  test_that("happy_df_csv should be a data frame.", {
    expect_true("data.frame" %in% class(happy_df_csv))
  })
  test_that("happy_df_csv does not contain the correct data.", {
    expect_equal(dim(happy_df_csv), c(1562, 19))
    expect_equal(digest(int_round(sum(happy_df_csv$year), 2)), 'b3c7645d3b175832c58747e47a9c7bff')
  })
  print("Success!")
}

test_1.3.1 <- function() {
  test_that("Did not create an object named happy_df", {
    expect_true(exists("happy_df"))
  })
  test_that("happy_df should be a data frame.", {
    expect_true("data.frame" %in% class(happy_df))
  })
  test_that("happy_df does not contain the correct data.", {
    expect_equal(dim(happy_df), c(1562, 19))
    expect_equal(digest(int_round(sum(happy_df$year), 2)), 'b3c7645d3b175832c58747e47a9c7bff')
  })
  print("Success!")
}

test_1.3.2 <- function() {
  test_that("Did not create an object named happy_step1", {
    expect_true(exists("happy_step1"))
  })
  test_that("Did not create an object named happy_step2", {
    expect_true(exists("happy_step2"))
  })
  test_that("Did not create an object named reduced_happy_df", {
    expect_true(exists("reduced_happy_df"))
  })
  test_that("reduced_happy_df does not contain the correct number of rows and/or columns.", {
    expect_setequal(dim(reduced_happy_df), c(141, 2))
  })
  test_that("The year column in reduced_happy_df should only contain 2017", {
    expect_equal(int_round(unique(happy_step1$year), 0), 2017)
  })
  test_that("Columns in reduced_happy_df contain incorrect values.", {
    expect_equal(digest(int_round(sum(reduced_happy_df$Positive.affect.scaled, na.rm = TRUE), 2)), '0a667e2dfc8faca592dde223c1ec3dec')
    expect_equal(digest(int_round(sum(reduced_happy_df$Healthy.life.expectancy.at.birth, na.rm = TRUE), 2)), '90a8884408be62b74a0558ab7fb40db0')
  })
  print("Success!")
}

test_1.4 <- function() {
  test_that("Did not create a plot named happy_plot", {
    expect_true(exists("happy_plot"))
  })
  properties <- c(happy_plot$layers[[1]]$mapping, happy_plot$mapping)

  test_that("Healthy.life.expectancy.at.birth should be on the x-axis.", {
    expect_true("Healthy.life.expectancy.at.birth" == rlang::get_expr(properties$x))
  })
  test_that("Positive.affect.scaled should be on the y-axis.", {
    expect_true("Positive.affect.scaled" == rlang::get_expr(properties$y))
  })
  test_that("happy_plot should be a scatter plot.", {
    expect_true("GeomPoint" %in% c(class(happy_plot$layers[[1]]$geom)))
  })
  test_that("Labels on the axes should be descriptive and human readable.", {
    expect_false(happy_plot$labels$y == "Positive.affect.scaled")
    expect_false(happy_plot$labels$x == "Healthy.life.expectancy.at.birth")
  })
  print("Success!")
}

test_2.1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(as.character(answer2.1)), "475bf9280aab63a82af60791302736f6")
  })
  print("Success!")
}

test_2.2.0 <- function() {
  test_that("Did not create an object named whistler_2018", {
    expect_true(exists("whistler_2018"))
  })
  test_that("whistler_2018 should be a data frame.", {
    expect_true("data.frame" %in% class(whistler_2018))
  })
  test_that("whistler_2018 does not contain the correct number of rows and/or columns.", {
    expect_equal(dim(whistler_2018), c(365, 27))
  })
  test_that("whistler_2018 does not contain the correct data.", {
    expect_equal(digest(int_round(sum(whistler_2018$`Total Snow (cm)`, na.rm = TRUE), 2)), '2f5608a20784d5f19ac88237c04a1e10')
  })
  print("Success!")
}

test_2.2.1 <- function() {
  test_that("whistler_2018 does not contain the correct number of rows and/or columns.", {
    expect_equal(dim(whistler_2018), c(365, 27))
  })
  test_that("whistler_2018 still contains white space in its column names.", {
    expect_equal(int_round(length(grep(pattern = " ", x = colnames(whistler_2018))), 0), 0)
  })
  print("Success!")
}

test_2.3 <- function() {
  test_that("Did not create a plot named whistler_2018_plot", {
    expect_true(exists("whistler_2018_plot"))
  })
  properties <- c(whistler_2018_plot$layers[[1]]$mapping, whistler_2018_plot$mapping)

  test_that("Date.Time should be on the x-axis.", {
    expect_true("Date.Time" == rlang::get_expr(properties$x))
  })
  test_that("Total.Snow..cm. should be on the y-axis.", {
    expect_true("Total.Snow..cm." == rlang::get_expr(properties$y))
  })
  test_that("whistler_2018_plot should be a line plot.", {
    expect_true("GeomLine" %in% c(class(whistler_2018_plot$layers[[1]]$geom)))
  })
  test_that("Labels on the axes should be descriptive and human readable.", {
    expect_false(whistler_2018_plot$labels$y == "Total.Snow..cm.")
    expect_false(whistler_2018_plot$labels$x == "Date.Time")
  })
  print("Success!")
}

test_3.0 <- function() {
  test_that("Did not create a database connected called connection", {
    expect_true(exists("connection"))
    expect_equal(digest(class(connection)), "df66af2493a2405d1c7763f4bfbfebc0")
  })
  test_that("Did not create a table called project_data", {
    expect_true(exists("project_data"))
    expect_equal(digest(class(project_data))[[1]], "98f4e628b20def80372d0acca8b5edb0")
  })
  test_that("The project_data table has the wrong information in it", {
    expect_equal(digest(colnames(project_data)[[13]]), "fd0b87dfaa67d4c90844080d5d9355ec")
    expect_equal(digest(colnames(project_data)[[14]]), "2f2ec36ffb7792b1f8e274387e520edd")
    expect_equal(digest(int_round(length(colnames(project_data)), 0)), "4d4c1ad2286f1a7670a024467dd10808")
    expect_equal(digest(round(as_tibble(summarize(project_data, sum = sum(goal, na.rm = TRUE)))[[1]], 0)), '46e3118b0210dec665d9c8dacc68aacb') # cant apply int_round
  })
  print("Success!")
}

test_3.1 <- function() {
  test_that("Incorrect answer", {
    expect_equal(digest(answer3.1), "3a5505c06543876fe45598b5e5e5195d")
  })
  print("Success!")
}


test_3.2 <- function() {
  test_that("Did not create a plot named funding_over_time_plot", {
    expect_true(exists("funding_over_time_plot"))
  })
  test_that("The axis labels are incorrect", {
    expect_equal(digest(funding_over_time_plot$labels$x), "bd1055a7d6af03e74a4462edbc5b35c4")
    expect_equal(digest(funding_over_time_plot$labels$y), "48c36ecb38a110dffe3f40add37ccdc1")
  })
  test_that("The x/y data are incorrect", {
    expect_equal(digest(funding_over_time_plot$mapping$x), "c5a397ebe58129a88408811144869370")
    expect_equal(digest(funding_over_time_plot$mapping$y), "5d75d853465e03f6828b25a1808b073b")
  })
  test_that("arrival_delay_plot should be a scatter plot", {
    expect_equal(digest(class(funding_over_time_plot$layers[[1]]$geom)[[1]]), "911e5b9debfb523f25ad2ccc01a4b2dd")
  })
  print("Success!")
}



test_3.4 <- function() {
  test_that("Did not create project_data.csv in the data/ folder.", {
    expect_true(file.exists("data/project_data.csv"))
  })
  test_that("the project_data.csv file does not contain the right data", { # read in data file as a string, check student's data = that string
    expect_equal(digest(file.info("data/project_data.csv")$size), "5cb5f9f655e59768877bd995511acb2c")
  })
  test_that("the project_df object does not exist", {
    expect_true(exists("project_df"))
  })
  test_that("the project_df object does not have the right number of rows & columns", {
    expect_equal(digest(int_round(nrow(project_df), 0)), 'bef07e5e73d03523a0385aa9c06dadb4')
    expect_equal(digest(int_round(ncol(project_df), 0)), '234a2a5581872457b9fe1187d1616b13')
  })
  test_that("the project_df object does not have the right data", {
    expect_equal(digest(int_round(sum(project_df$goal), 0)), 'e5730c99d7f2abf710e5ebbdb4738bff')
  })
  print("Success!")
}
