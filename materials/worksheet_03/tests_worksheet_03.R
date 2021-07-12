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

test_0.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer0.0)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }


test_0.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer0.1)), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_0.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer0.2)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_0.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer0.3)), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer1.1)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer1.2)), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.3 <- function(){
    test_that('Did not create an object named avocado', {
        expect_true(exists("avocado")) 
        })
    test_that('avocado should be a data frame.', {
        expect_true('data.frame' %in% class(avocado))
        })
    test_that('avocado does not contain the correct data.', {
        expect_equal(dim(avocado), c(17911, 9))
        expect_equal(digest(int_round(sum(avocado$average_price), 2)), '925e36908ff4830300330ada3666458c') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(avocado), c("Date", "average_price", "small_hass_volume", "large_hass_volume", "extra_l_hass_volume", "type", "yr", "region", "wk"))
        })
print("Success!")
    }

test_1.4 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer1.4)), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.5 <- function(){
    test_that('Did not create a plot named avocado_plot', {
        expect_true(exists("avocado_plot")) 
        })
    properties <- c(avocado_plot$layers[[1]]$mapping, avocado_plot$mapping)
    test_that('Date should be on the x-axis.', {
        expect_true("Date" == rlang::get_expr(properties$x))
        })
    test_that('average_price should be on the y-axis.', {
        expect_true("average_price" == rlang::get_expr(properties$y))
        })
    test_that('avocado_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(avocado_plot$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(avocado_plot$labels$y == 'average_price')
        })
print("Success!")
    }

test_1.6 <- function(){
    test_that('Did not create an object named avocado_aggregate', {
        expect_true(exists("avocado_aggregate")) 
        })
    test_that('avocado_aggregate does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(avocado_aggregate), c(53, 2))
        })
    test_that('Columns in avocado_aggregate contain incorrect values.', {
        expect_equal(digest(int_round(sum(avocado_aggregate$wk), 2)), '52e8770115417a331e4e2a539238bbf1') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(avocado_aggregate$average_price), 2)), '2a69e8ab5af131a648f5cb2d24e6fbcf') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.7 <- function(){
    test_that('Did not create a plot named avocado_aggregate_plot', {
        expect_true(exists("avocado_aggregate_plot")) 
        })
    properties <- c(avocado_aggregate_plot$layers[[1]]$mapping, avocado_aggregate_plot$mapping)
    test_that('week should be on the x-axis.', {
        expect_true("wk" == rlang::get_expr(properties$x))
        })
    test_that('average_price should be on the y-axis.', {
        expect_true("average_price" == rlang::get_expr(properties$y))
        })
    test_that('avocado_aggregate_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(avocado_aggregate_plot$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(avocado_aggregate_plot$labels$y == 'average_price')
        expect_false(avocado_aggregate_plot$labels$x == 'wk')
        })
print("Success!")
    }

test_1.8 <- function(){
    test_that('Did not create an object named avocado', {
        expect_true(exists("avocado")) 
        })
    test_that('avocado does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(avocado), c(17911, 10))
        })
    test_that('Columns in avocado contain incorrect values.', {
        expect_equal(digest(int_round(sum(avocado$total_volume, na.rm = TRUE), -2)), '7128826a55bfba26d8c2df46ab65dbcf') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(avocado$average_price, na.rm = TRUE), 2)), '925e36908ff4830300330ada3666458c') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.9 <- function(){
    test_that('Did not create an object named avocado_aggregate_2', {
        expect_true(exists("avocado_aggregate_2")) 
        })
    test_that('avocado_aggregate_2 does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(avocado_aggregate_2), c(53, 2))
        })
    test_that('Columns in avocado_aggregate_2 contain incorrect values.', {
        expect_equal(digest(int_round(sum(avocado_aggregate_2$total_volume), 2)), '31b3d41174825b72e1d523622ee021e6') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(avocado_aggregate_2$wk), 2)), '52e8770115417a331e4e2a539238bbf1') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_1.10 <- function(){
    test_that('Did not create a plot named avocado_aggregate_plot_2', {
        expect_true(exists("avocado_aggregate_plot_2")) 
        })
     properties <- c(avocado_aggregate_plot_2$layers[[1]]$mapping, avocado_aggregate_plot_2$mapping)
    test_that('week should be on the x-axis.', {
        expect_true("wk" == rlang::get_expr(properties$x))
        })
    test_that('total_volume should be on the y-axis.', {
        expect_true("total_volume" == rlang::get_expr(properties$y))
        })
    test_that('avocado_aggregate_plot_2 should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(avocado_aggregate_plot_2$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(avocado_aggregate_plot_2$labels$y == 'total_volume')
        expect_false(avocado_aggregate_plot_2$labels$x == 'wk')
        })
print("Success!")
    }

test_2.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.1), 'd2a90307aac5ae8d0ef58e2fe730d38b') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.2), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.3 <- function(){
    test_that('Did not create an object named sea_surface', {
        expect_true(exists("sea_surface")) 
        })
    test_that('sea_surface should be a data frame.', {
        expect_true('data.frame' %in% class(sea_surface))
        })
    test_that('sea_surface does not contain the correct data.', {
        expect_equal(dim(sea_surface), c(105, 13))
        expect_equal(digest(int_round(sum(sea_surface$Year), 2)), 'f5530ffd8438e19b12730e111c4e2cc6') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(sea_surface), c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        })
print("Success!")
    }

test_2.3.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer2.3.1)), '01a75cb73d67b0f895ff0e61449c7bf8') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }


test_2.4 <- function(){
    test_that('Did not create an object named tidy_temp', {
        expect_true(exists("tidy_temp")) 
        })
    test_that('tidy_temp does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(tidy_temp), c(1260, 3))
        })
    test_that('Columns in tidy_temp contain incorrect values.', {
        expect_equal(digest(int_round(sum(tidy_temp$Temperature, na.rm = TRUE), 2)), 'ffc4ca6bf87040a2a3f7f80c32c15def') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_2.5 <- function(){
    test_that('Did not create a plot named nov_temp_plot', {
        expect_true(exists("nov_temp_plot")) 
        })
    properties <- c(nov_temp_plot$layers[[1]]$mapping, nov_temp_plot$mapping)
    test_that('The Month column in nov_temp_plot should only contain November.', {
        expect_equal(unique(nov_temp_plot$data$Month), "Nov")
        })
    test_that('Year should be on the x-axis.', {
        expect_true("Year" == rlang::get_expr(properties$x))
        })
    test_that('Temperature should be on the y-axis.', {
        expect_true("Temperature" == rlang::get_expr(properties$y))
        })
    test_that('nov_temp_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(nov_temp_plot$layers[[1]]$geom)))
        })
print("Success!")
    }

test_2.6 <- function(){
    test_that('Did not create a plot named all_temp_plot', {
        expect_true(exists("all_temp_plot")) 
        })
    properties <- c(all_temp_plot$layers[[1]]$mapping, all_temp_plot$mapping)
    test_that('Need to use tidy_temp as the data!', {
        expect_true("Month" %in% colnames(all_temp_plot$data))
        })
    test_that('Should use facet_wrap to facet by month', {
        expect_true('FacetWrap' %in% class(all_temp_plot$facet)) 
        })
    test_that('Year should be on the x-axis.', {
        expect_true("Year" == rlang::get_expr(properties$x))
        })
    test_that('Temperature should be on the y-axis.', {
        expect_true("Temperature" == rlang::get_expr(properties$y))
        })
    test_that('all_temp_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(all_temp_plot$layers[[1]]$geom)))
        })
print("Success!")
    }

test_3.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer3.1)), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_3.2 <- function(){
    test_that('Did not create an object named madrid', {
        expect_true(exists("madrid")) 
        })
    test_that('madrid should be a data frame.', {
        expect_true('data.frame' %in% class(madrid))
        })
    test_that('madrid does not contain the correct data.', {
        expect_equal(dim(madrid), c(51864, 17))
        expect_equal(digest(int_round(sum(madrid$BEN, na.rm = TRUE), 2)), 'fc1f955cfd95bfb1528ba72b1d7dbf85') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(madrid), c("date", "BEN", "CO", "EBE", "MXY", "NMHC", "NO_2", "NOx", "OXY", "O_3", "PM10", "PXY", "SO_2", "TCH", "TOL", "year", "mnth"))
        })
print("Success!")
    }

test_3.3 <- function(){
    test_that('Did not create a plot named EBE_pollution', {
        expect_true(exists("EBE_pollution")) 
        })
    properties <- c(EBE_pollution$layers[[1]]$mapping, EBE_pollution$mapping)
    test_that('date should be on the x-axis.', {
        expect_true("date" == rlang::get_expr(properties$x))
        })
    test_that('EBE should be on the y-axis.', {
        expect_true("EBE" == rlang::get_expr(properties$y))
        })
    test_that('EBE_pollution should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(EBE_pollution$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(EBE_pollution$labels$y == 'EBE')
        expect_false(EBE_pollution$labels$x == 'date')
        })
print("Success!")
    }

test_3.4 <- function(){
    test_that('Did not create an object named madrid_pollution', {
        expect_true(exists("madrid_pollution")) 
        })
    test_that('madrid_pollution does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(madrid_pollution), c(72, 3))
        })
    test_that('Columns in avocado_aggregate_2 contain incorrect values.', {
        expect_equal(digest(int_round(sum(madrid_pollution$year), 2)), 'a23f8380b73a23a2f7ad10600a6a4829') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_3.5 <- function(){
    test_that('Did not create a plot named madrid_plot', {
        expect_true(exists("madrid_plot")) 
        })
    properties <- c(madrid_plot$layers[[1]]$mapping, madrid_plot$mapping)
    test_that('month should be on the x-axis.', {
        expect_true("mnth" == rlang::get_expr(properties$x))
        })
    test_that('max_ebe should be on the y-axis.', {
        expect_true("max_ebe" == rlang::get_expr(properties$y))
        })
    test_that('madrid_plot should be a scatter plot.', {
        expect_true("GeomPoint" %in% c(class(madrid_plot$layers[[1]]$geom)))
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_false(madrid_plot$labels$y == 'max_ebe')
        expect_false(madrid_plot$labels$x == 'mnth')
        })
print("Success!")
    }

test_3.6 <- function(){
    test_that('Did not create an object named pollution_2001', {
        expect_true(exists("pollution_2001")) 
        })
    test_that('pollution_2001 does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(pollution_2001), c(1, 14))
        })
    test_that('Columns in pollution_2001 contain incorrect values.', {
        expect_equal(digest(int_round(pollution_2001$MXY, 2)), '9e56083b25f55a915ef285e242392a4a')  # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(pollution_2001), 2)), 'f12c2d5045ab522d0268bc7d14b17290')  # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_3.7 <- function(){
    test_that('Did not create an object named pollution_2006', {
        expect_true(exists("pollution_2006")) 
        })
    test_that('pollution_2006 does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(pollution_2006), c(1, 14))
        })
    test_that('Columns in pollution_2006 contain incorrect values.', {
        expect_equal(digest(int_round(pollution_2006$MXY, 2)), 'd1e716c1361c7b0e57cc9f66a87e1bfb')  # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(int_round(sum(pollution_2006), 2)), 'a49327566693ae18684d9e8618ce48a1')  # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_3.8 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer3.8)), '1ce38a3fa8946d5768f4fc87b739ec31') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_3.9 <- function(){
    test_that('Did not create an object named pollution_diff', {
        expect_true(exists("pollution_diff")) 
        })
    test_that('pollution_diff does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(pollution_diff), c(14, 2))
        })
    test_that('pollution_diff does not contain the correct columns: pollutant and value.', {
        expect_equal(colnames(pollution_diff), c('pollutant', 'value'))
        })
    test_that('Columns in pollution_diff contain incorrect values.', {
        expect_equal(digest(int_round(sum(pollution_diff$value), 2)), 'b6181c536435d6429d51eebb59563fb7') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }

test_3.10 <- function(){
    test_that('Did not create an object named max_pollution_diff', {
        expect_true(exists("max_pollution_diff")) 
        })
    test_that('max_pollution_diff does not contain the correct number of rows and/or columns.', {
        expect_equal(dim(max_pollution_diff), c(1, 2))
        })
    test_that('max_pollution_diff does not contain the correct columns: pollutant and value.', {
        expect_equal(colnames(max_pollution_diff), c('pollutant', 'value'))
        })
    test_that('Columns in pollution_diff contain incorrect values.', {
        expect_equal(digest(int_round(sum(max_pollution_diff$value), 2)), '44ee23cae87e4fb16d02379e8b8536bc') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
    }
