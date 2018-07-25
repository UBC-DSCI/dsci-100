# DSCI 100 Lecture learning objectives

## 1. Introduction to Data Science
By the end of the chapter, students will be able to:

* use a Jupyter notebook to execute provided R code
* edit code and markdown cells in a Jupyter notebook
* create new code and markdown cells in a Jupyter notebook
* load the `tidyverse` library into R
* create new variables and objects in R using the assignment symbol
* use the help and documentation tools in R
* match the names of the following functions from the `tidyverse` library to their documentation descriptions: 
    - `read_csv` 
    - `select`
    - `mutate`
    - `filter`
    - `ggplot`
    - `aes`
* chain together two functions using the pipe operator, `%>%`

## 2. Reading in data locally and from the web
By the end of the chapter, students will be able to:

* define the following:
    - absolute file path
    - relative file path
    - url
* discuss why relative paths should be used over absolute paths in a Jupyter notebook
* match the following `tidyverse` `read_*` function argument descriptions to a simplified version of them:
    - `file`
    - `delim`
    - `col_names`
    - `skip`
* write a relative file path for the `file` argument of `tidyverse` `read_*` functions
* choose the appropriate `tidyverse` `read_*` function and function arguments to load a given tabular data set into R
* use the `rvest` `html_nodes`, `html_text` and `html_attr` functions to scrape data from a `.html` file on the web
* compare downloading tabular data from a plain text file (e.g., `.csv`) from the web versus scraping data from a `.html` file

## 3. Cleaning and wrangling data
By the end of the chapter, students will be able to:

* define the term "tidy data"
* discuss the advantages and disadvantages from storing data in a tidy data format
* recall and use the following tidyverse functions and operators for their intended data wrangling tasks:
    - `select`
    - `filter`
    - `map`
    - `mutate`
    - `summarise`
    - `group_by`
    - `gather`
    - `%in%`
 
## 4. Effective data visualization
By the end of the chapter, students will be able to:

* Define the three key aspects of ggplot objects:
     - aesthetic mappings
     - geometric objects
     - scales
* Use the `ggplot2` function in R to create the following visualizations:
    - 2-D scatter plot
    - 2-D scatter plot with a third variable that stratifies the groups
    - count bar chart for multiple groups
    - proportion bar chart for multiple groups
    - stacked bar chart for multiple groups
* List the rules of thumb for effective visualizations 
* Given a visualization and a sentence describing it's intended task, evaluate it's effectiveness and suggest ways to improve the visualization with respect to that intended task

## 5. Classification
By the end of the chapter, students will be able to:

* Recognize situations where a simple classifier would be appropriate for making predictions.
* Explain the k-nearest neighbour classification algorithm.
* Interpret the output of a classifier.
* Compute, by hand, the distance between points when there are two attributes.
* Describe what a training data set is and how it is used in classification.
* In a dataset with two attributes, perform k-nearest neighbour classification in R using `caret::train(method = "knn", ...)` to predict the class of a single new observation.

## 6. Classification, continued
By the end of the chapter, students will be able to:

* Describe what a test data set is and how it is used in classification.
* Using R, evaluate k-nn classification accuracy using a test data set and appropriate metrics.
* Using R, execute cross-validation in R to choose the number of neighbours for k-nn classification.
* Identify when it is necessary to scale variables before classification and do this using R
* In a dataset with > 2 attributes, perform k-nearest neighbour classification in R using `caret::train(method = "knn", ...)` to predict the class of a test dataset.
* Describe advantages and disadvantages of the k-nearest neighbour classification algorithm.

## 7. Clustering

By the end of the chapter, students will be able to:

* Describe a case where clustering would be an appropriate tool, and what insight it would bring from the data.
* Explain the kmeans clustering algorithm.
* Interpret the output of a kmeans cluster analysis.
* Perform kmeans clustering in R using `kmeans`.
* Visualize the output of kmeans clustering in R using pair-wise scatter plots.
* Identify when it is necessary to scale variables before clustering and do this using R.
* Use the elbow method to choose the number of clusters for k-means.
* Describe advantages, limitations and assumptions of the kmeans clustering algorithm.


## 8. Regression
By the end of the chapter, students will be able to:

* Recognize situations where a simple regression analysis would be appropriate for making predictions.
* Explain the k-nearest neighbour (k-nn) regression algorithm and describe how it differs from k-nn classification.
* Interpret the output of a k-nn regression.
* In a dataset with two variables, perform k-nearest neighbour regression in R using `caret::knnregTrain()` to predict the values for a test dataset.
* Using R, execute cross-validation in R to choose the number of neighbours.
* Using R, evaluate k-nn regression prediction accuracy using  a test data set and an appropriate metric (*e.g.*, means square prediction error).
* Describe advantages and disadvantages of the k-nearest neighbour regression approach.

## 9. Regression, continued
By the end of the chapter, students will be able to:

* In the context of k-nn regression, compare and contrast goodness of fit and prediction properties (namely MSE vs MSPE).
* In a dataset with 2 variables, perform simple ordinary least squares regression in R using `lm()` to predict the values for a test dataset.
* Compare and contrast predictions obtained from k-nearest neighbour regression to those obtained using simple ordinary least squares regression from the same dataset.

## 10. Regression, continued some more...

By the end of the chapter, students will be able to:

* Explain in words or using drawings, how bootstrap samples can be generated from a single sample.
* From a single sample, generate multiple samples through bootstrapping using R.
* In R, overlay the ordinary least squares regression lines from multiple bootstrapped samples on a single plot. 
* Discuss what the collection of these slopes represents.
