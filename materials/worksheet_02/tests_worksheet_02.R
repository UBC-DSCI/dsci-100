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
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.1), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_1.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.2), '05ca18b596514af73f6880309a21b5dd') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_1.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(A), '6717f2823d3202449301145073ab8719') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(B), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(C), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(D), '6717f2823d3202449301145073ab8719') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(E), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(F), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test  
        })
print("Success!")
}

test_1.4 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer1.4), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_1.5 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(A), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(B), '6717f2823d3202449301145073ab8719') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(C), '5e338704a8e069ebd8b38ca71991cf94') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(D), 'dbc09cba9fe2583fb01d63c70e1555a8') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(E), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_2.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(G), 'db8e490a925a60e62212cefc7674ca02') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(H), 'e5b57f323c7b3719bbaaf9f96b260d39') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(I), '6717f2823d3202449301145073ab8719') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(J), 'dbc09cba9fe2583fb01d63c70e1555a8') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_2.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer2.2), '05ca18b596514af73f6880309a21b5dd') # we hid the answer to the test here so you can't see it, but we can still run the test   
        })
print("Success!")
}

test_2.3 <- function(){
    test_that('Solution is incorrect', {
        expect_that(exists('answer2.3'), is_true())
        expect_equal(digest(answer2.3), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_3.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.1), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_3.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.2), '3a5505c06543876fe45598b5e5e5195d') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_3.3 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(A), '0590b0427c1b19a6eb612d19888aa52f') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(B), 'e6d21e822242a5c1c9f58806024e77ba') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(C), 'f2375c071669f9b6399f28e06a598e57') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(D), '0590b0427c1b19a6eb612d19888aa52f') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(E), '863dfc36ab2bfe97404cc8fc074a5241') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(digest(F), '3697e3a900218295975a04d2205e3518') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}


test_3.4 <- function(){
    test_that('Did not create an object named happiness_report_path', {
        expect_true(exists("happiness_report_path")) 
        })
    test_that('Did not create an object named happiness_report', {
        expect_true(exists("happiness_report")) 
        })
    test_that('happiness_report should be a data frame.', {
        expect_true('data.frame' %in% class(happiness_report))
        })
    test_that('Did not use the shortest relative path', {
        expect_equal(digest(happiness_report_path),  'f82fd45a6bec54581361a0dfe13e9ea1')
        })
    test_that('happiness_report does not contain the correct information.', {
        expect_equal(dim(happiness_report), c(155, 5))
        expect_equal(digest(int_round(sum(happiness_report$freedom), 2)), 'fb345985a119a2b0f99a7896efa4d2c7') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(happiness_report), c("country", "happiness_score", "GDP_per_capita", "life_expectancy", "freedom"))
        })
print("Success!")
}

test_3.5 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.5), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_3.6.1 <- function(){
    test_that('Did not create an object named happy_semi_df', {
        expect_true(exists("happy_semi_df")) 
        })
    test_that('happy_semi_df should be a data frame.', {
        expect_true('data.frame' %in% class(happy_semi_df))
        })
    test_that('happy_semi_df does not contain the correct information.', {
        expect_equal(dim(happy_semi_df), c(155, 5))
        expect_equal(digest(int_round(sum(as.integer(sub(",", ".", happy_semi_df$freedom, fixed = TRUE))), 2)), '1473d70e5646a26de3c52aa1abd85b1f') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(happy_semi_df), c("country", "happiness_score", "GDP_per_capita", "life_expectancy", "freedom"))
        })
print("Success!")
}

test_3.6.2 <- function(){
    test_that('Did not create an object named happy_semi_df2', {
        expect_true(exists("happy_semi_df2")) 
        })
    test_that('happy_semi_df2 should be a data frame.', {
        expect_true('data.frame' %in% class(happy_semi_df2))
        })
    test_that('happy_semi_df2 does not contain the correct information.', {
        expect_equal(dim(happy_semi_df2), c(155, 5))
        expect_equal(digest(int_round(sum(happy_semi_df2$happiness_score), 2)), '5c379e06a33669ccfd7208cb56366ca2') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(happy_semi_df2), c("country", "happiness_score", "GDP_per_capita", "life_expectancy", "freedom"))
        })
print("Success!")
}

test_3.6.3 <- function(){
    test_that('Did not create an object named happy_tsv', {
        expect_true(exists("happy_tsv")) 
        })
    test_that('happy_tsv should be a data frame.', {
        expect_true('data.frame' %in% class(happy_tsv))
        })
    test_that('happy_tsv does not contain the correct information.', {
        expect_equal(dim(happy_tsv), c(155, 5))
        expect_equal(digest(int_round(sum(happy_tsv$freedom), 2)), 'fb345985a119a2b0f99a7896efa4d2c7') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(happy_tsv), c("country", "happiness_score", "GDP_per_capita", "life_expectancy", "freedom"))
        })
print("Success!")
}

test_3.6.4 <- function(){
    test_that('Did not create an object named happy_metadata', {
        expect_true(exists("happy_metadata")) 
        })
    test_that('happy_metadata should be a data frame.', {
        expect_true('data.frame' %in% class(happy_metadata))
        })
    test_that('happy_metadata does not contain the correct information.', {
        expect_equal(dim(happy_metadata), c(155, 5))
        expect_equal(digest(int_round(sum(happy_metadata$freedom), 2)), 'fb345985a119a2b0f99a7896efa4d2c7') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(happy_metadata), c("country", "happiness_score", "GDP_per_capita", "life_expectancy", "freedom"))
        })
print("Success!")
}

test_3.6.5 <- function(){
    test_that('Did not create an object named happy_header', {
        expect_true(exists("happy_header")) 
        })
    test_that('happy_header should be a data frame.', {
        expect_true('data.frame' %in% class(happy_header))
        })
    test_that('happy_header does not contain the correct information.', {
        expect_equal(dim(happy_header), c(155, 5))
        expect_equal(digest(int_round(sum(happy_header$freedom), 2)), 'fb345985a119a2b0f99a7896efa4d2c7') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(happy_header), c("country", "happiness_score", "GDP_per_capita", "life_expectancy", "freedom"))
        })
print("Success!")
}

test_3.7 <- function(){
    test_that('Did not create an object named happy_xlsx', {
        expect_true(exists("happy_xlsx")) 
        })
    test_that('happy_xlsx should be a data frame.', {
        expect_true('data.frame' %in% class(happy_xlsx))
        })
    test_that('happy_xlsx does not contain the correct information.', {
        expect_equal(dim(happy_xlsx), c(155, 5))
        expect_equal(digest(int_round(sum(happy_xlsx$freedom), 2)), 'fb345985a119a2b0f99a7896efa4d2c7') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_equal(colnames(happy_xlsx), c("country", "happiness_score", "GDP_per_capita", "life_expectancy", "freedom"))
        })
print("Success!")
}

test_3.8 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer3.8), '0590b0427c1b19a6eb612d19888aa52f') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_3.9 <- function(){
    test_that('Did not create a plot named header_plot', {
        expect_true(exists("header_plot")) 
        })
    
    properties <- c(header_plot$layers[[1]]$mapping, header_plot$mapping)
    labels <- header_plot$labels
    
    test_that('GDP_per_capita should be on the x-axis.', {
        expect_true("GDP_per_capita" == rlang::get_expr(properties$x))
        })
    test_that('life_expectancy should be on the y-axis.', {
        expect_true("life_expectancy" == rlang::get_expr(properties$y))
        })
    test_that('header_plot should be a scatter plot.', {
        expect_that("GeomPoint" %in% c(class(header_plot$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((header_plot$labels$x) == 'GDP_per_capita', is_false())
        expect_that((header_plot$labels$y) == 'life_expectancy', is_false())
        })
print("Success!")
}


test_4.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer4.1), 'd2a90307aac5ae8d0ef58e2fe730d38b') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}


test_4.2 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(answer4.2), 'c1f86f7430df7ddb256980ea6a3b57a4') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_4.3.1 <- function(){
    test_that('Did not create the conn object', {
        expect_true(exists("conn")) 
        })
    test_that('conn is not an RSQLite database connection object', {
        expect_equal(digest(class(conn)), 'db665ff04541c433e1f30318e7715ef4')
        })
    test_that('conn was loaded from the wrong file',{
        expect_true(grepl('data/flights_filtered.db', format(conn)))
        })
    test_that('conn is the wrong database',{
        expect_equal(digest(dbListTables(conn)), '51a67410d71bf270db1994ea905cda13')
        })
print("Success!")
}

test_4.3.2 <- function(){
        test_that('table name is incorrect',{
        expect_equal(digest(flights_table_name), '51a67410d71bf270db1994ea905cda13')
        })
 print('Success!')
}

test_4.3.3 <- function(){
    test_that('Did not create an object named flight_data', {
        expect_true(exists("flight_data")) 
        })
    test_that('flight_data should be a data frame.', {
        expect_true('tbl_SQLiteConnection' %in% class(flight_data))
        })
    test_that('flight_data does not contain the correct information.', {
        expect_equal(digest(int_round(as_tibble(tally(flight_data))[[1]], 2)), '6bf4cc4788963306e4991f74f4edfd06')
        expect_equal(digest(int_round(dim(flight_data)[2], 2)), '306a937dfa0335e74514e4c6044755f6')
        expect_equal(digest(int_round(as_tibble(summarize(flight_data, sum=sum(ARRIVAL_DELAY)))[[1]], 2)), '18935ad0b2d8d671ed44f695a1324b62')
        expect_equal(colnames(flight_data), c('YEAR', 'MONTH', 'DAY', 'DAY_OF_WEEK', 'ORIGIN_AIRPORT', 'DESTINATION_AIRPORT', 'DISTANCE', 'SCHEDULED_DEPARTURE', 'DEPARTURE_DELAY', 'SCHEDULED_ARRIVAL', 'ARRIVAL_DELAY', 'DIVERTED', 'CANCELLED'))
        })

print("Success!")
}

test_4.4 <- function(){
    test_that('Did not create an object named delay_data', {
        expect_true(exists("delay_data")) 
        })
    test_that('delay_data has the wrong dimensions', {
        expect_equal(digest(int_round(as_tibble(tally(delay_data))[[1]], 2)), 'c73696e1f5ebc74021271a447e6a027d')
        expect_equal(digest(int_round(dim(delay_data)[2], 2)), '2567f3d5adc004a73dc268884026f3bd')
     })
    test_that('delay data has the wrong variables', {
    expect_setequal(colnames(delay_data), c('DEPARTURE_DELAY', 'ARRIVAL_DELAY'))
     })
print('Success!')
}

test_4.5 <- function(){
    test_that('Did not create a plot named arrival_delay_plot', {
        expect_true(exists("arrival_delay_plot")) 
        })
    test_that('The axis labels are incorrect', {
        expect_equal(digest(arrival_delay_plot$labels$x), "721f8fe183a39e61a2140d9dd01b6042")
        expect_equal(digest(arrival_delay_plot$labels$y), '9ae00562a72e7b3aa91a39269590839a')
        })
    test_that('The x/y data are incorrect', {
        expect_equal(digest(arrival_delay_plot$mapping$x), '550b90233ab71397604016f5474f572f')
        expect_equal(digest(arrival_delay_plot$mapping$y), 'f9e884084b84794d762a535f3facec85')
        })
    test_that('arrival_delay_plot should be a histogram', {
        expect_equal(digest(class(arrival_delay_plot$layers[[1]]$geom)[[1]]), '00af9702c1de4491cd9d59e3c94080a8') 
     })
print("Success!")
}

test_4.6 <- function(){
    test_that('Did not create a plot named departure_delay_plot', {
        expect_true(exists("departure_delay_plot")) 
        })
    test_that('The axis labels are incorrect', {
        expect_equal(digest(departure_delay_plot$labels$x), "721f8fe183a39e61a2140d9dd01b6042")
        expect_equal(digest(departure_delay_plot$labels$y), '9ae00562a72e7b3aa91a39269590839a')
        })
    test_that('The x/y data are incorrect', {
        expect_equal(digest(departure_delay_plot$mapping$x), 'c92e510b14ce856cce6eb14e740686b1')
        expect_equal(digest(departure_delay_plot$mapping$y), 'f9e884084b84794d762a535f3facec85')
        })
    test_that('departure_delay_plot should be a histogram', {
        expect_equal(digest(class(departure_delay_plot$layers[[1]]$geom)[[1]]), '00af9702c1de4491cd9d59e3c94080a8') 
        })
print("Success!")
}

test_4.7 <- function(){
    test_that('Answer incorrect', {
        expect_equal(digest(answer4.7), '7d9e25abb82fe492a7007d36124cee47')
        })
print('Success!')
}

test_4.8.1 <- function(){
    test_that('Answer incorrect', {
        expect_equal(digest(answer4.8.1), '75f1160e72554f4270c809f041c7a776')
        })
print('Success!')
}


test_4.8.2 <- function(){
    test_that('Did not create the delay_dataframe object', {
        expect_true(exists("delay_dataframe"))
        })
    test_that("The delay_dataframe doesn't have the right data in it", {
        expect_equal(digest(int_round(sum(delay_dataframe), 2)), 'd142b116cb76f4e5c3fab6e96bd89a22')
        })
    test_that('Did not create the delay_data.csv file', {
        expect_true(file.exists('data/delay_data.csv'))
    })
    test_that('delay_data.csv does not have the right data in it', {
        expect_equal(digest(int_round(sum(read_csv('data/delay_data.csv')), 2)), 'd142b116cb76f4e5c3fab6e96bd89a22')
    })
print('Success!')
}

test_5.1.0 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer5.1.0)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_5.1.1 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer5.1.1)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}

test_5.2 <- function(){
    test_that('Did not create an object named gwp', {
        expect_true(exists("gwp")) 
        })
test_that('gwp should not be a data frame.', {
        expect_false('data.frame' %in% class(gwp))
        })
test_that('gwp does not contain the correct information.', {
        expect_equal(digest(int_round(length(gwp), 2)), '2567f3d5adc004a73dc268884026f3bd') # we hid the answer to the test here so you can't see it, but we can still run the test
        expect_true('xml_document' %in% attributes(gwp)$class)
        })
print("Success!")
}

test_5.4 <- function(){
    test_that('Did not create an object named gwp_value', {
        expect_true(exists("gwp_value")) 
        })
    test_that('gwp_value should be a vector containing characters.', {
        expect_that(is.vector(gwp_value), is_true())
        expect_equal(class(gwp_value), "character")
        })
print("Success!")
}

test_5.5 <- function(){
    test_that('Did not create an object named gwp', {
        expect_true(exists("gwp")) 
        })
    test_that('gwp should be a data frame.', {
        expect_true('data.frame' %in% class(gwp))
        })
    test_that('gwp should contain the columns: year and gwp_value', {
        expect_that("year" %in% colnames(gwp), is_true())
        expect_that("gwp_value" %in% colnames(gwp), is_true())
        })
print("Success!")
}

test_5.6 <- function(){
    test_that('Did not create a plot named gwp_historical', {
        expect_true(exists("gwp_historical")) 
        })
    
    properties <- c(gwp_historical$layers[[1]]$mapping, gwp_historical$mapping)
    labels <- gwp_historical$labels
    
    test_that('sqrt_year should be on the x-axis.', {
        expect_true("sqrt_year" == rlang::get_expr(properties$x))
        })
    test_that('gwp_value should be on the y-axis.', {
        expect_true("gwp_value" == rlang::get_expr(properties$y))
        })
    test_that('gwp_historical should be a line plot.', {
        expect_that("GeomLine" %in% c(class(gwp_historical$layers[[1]]$geom)) , is_true())
        })
    test_that('Labels on the axes should be descriptive and human readable.', {
        expect_that((gwp_historical$labels$y) == 'gwp_value', is_false())
        expect_that((gwp_historical$labels$x) == 'sqrt_year', is_false())
        })
print("Success!")
}

test_5.7 <- function(){
    test_that('Solution is incorrect', {
        expect_equal(digest(as.character(answer5.7)), '475bf9280aab63a82af60791302736f6') # we hid the answer to the test here so you can't see it, but we can still run the test
        })
print("Success!")
}
