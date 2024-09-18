# ----------------------- Helper Functions to Implement ------------------------

#' Evaluate whether the argument is less than 2
#'
#' Returns TRUE if the numeric argument x is a prime number, otherwise returns
#' FALSE
#'
#' @param x (numeric): the numeric value(s) to test
#'
#' @return logical value or vector indicating whether the numeric argument is less than 2
#' @export
#'
#' @examples
#' less_than_zero(-1)
#' [1] TRUE
#' less_than_zero(10)
#' [1] FALSE
#' less_than_zero(c(-1,0,1,2,3,4))
#' [1] TRUE FALSE FALSE FALSE FALSE FALSE
#' Here the only input of the function is x
#' if the input argument of x is less than zero: return TRUE
#' if the input argument of x is more than zero: return FALSE
less_than_zero <- function(x) {
  return(x < 0)
}

#' Evaluate whether the argument is between two numbers
#'
#' Returns TRUE if the numeric argument x is contained within the open interval
#' (a, b), otherwise return FALSE.
#'
#' hello is this working??
#' @param x (numeric): the numeric value(s) to test
#' @param a (number): the lower bound
#' @param b (number): the upper bound
#'
#' @return logical value of same type as input (e.g. scalar, vector, or matrix)
#' @export
#'
#' @examples
#' is_between(3,1,5)
#' [1] TRUE
#' is_between(c(1,9,5,2), 1, 5)
#' [1] FALSE FALSE FALSE TRUE
#' is_between(matrix(1:9, nrow=3, byrow=TRUE), 1, 5)
#'       [,1]  [,2]  [,3]
#' [1,] FALSE  TRUE  TRUE
#' [2,]  TRUE FALSE FALSE
#' [3,] FALSE FALSE FALSE
#' This function takes in three arguments: x, a, b
#' We want to know if x is between a and b
#' Using logical arguments, we must know if x ia greater than or equal to a.
#' X must also be less than or equal to b. 
#' If both conditions are met, then the functions returns TRUE
#' If one or both conditions are not met, then the function returns FALSE
is_between <- function(x, a, b) {
  # this is a simple logical operation in order to return TRUE, both statements must be TRUE
    return(x > a & x < b)
}

#' Return the values of the input vector that are not NA
#'
#' Returns the values of the input vector `x` that are not NA
#'
#' @param x (numeric): numeric vector to remove NA from 
#'
#' @return numeric vector `x` with all `NA` removed
#' @export
#'
#' @examples
#' x <- c(1,2,NA,3)
#' rm_na(x)
#' [1] 1 2 3
rm_na <- function(x) {
  #first we store the vector in another variable
  store_non_na <- c()
  #next we iterate through x
  for (i in 1:length(x)) {
    # now we ask whether the elements in the vector are not an NA value
    if (!is.na(x[i])) {
      #finally if the element is not an NA value, we add it to store_non_
      store_non_na <- c(store_non_na, x[i])
    }
  }
  #finally we return the values of the vector x that are not NA values
  return(store_non_na)
}

#' Calculate the median of each row of a matrix
#'
#' Given the matrix x with n rows and m columns, return a numeric vector of
#' length n that contains the median value of each row of x
#'
#' @param x (numeric matrix): matrix to compute median along rows
#'
#' @return (numeric vector) vector containing median row values
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' row_medians(m)
#' [1] 1 4 7
#' 
row_medians <- function(x) {
  # following will calculate the median of each row in a matrix. apply will help do this to every row
  # na.rm =  TRUE just means that we will not be operating on na values and will simply ignore them
  apply(x, 1, median, na.rm = TRUE)
}

#' Evaluate each row of a matrix with a provided function
#'
#' Given the matrix `x` with n rows and m columns, return a numeric vector of
#' length n that contains the returned values from the evaluation of the
#' function `fn` on each row. `fn` should be a function that accepts a vector as
#' input and returns a scalar value
#'
#' @param x (numeric matrix): matrix to evaluate the function along rows
#' @param fn (function) function that accepts a vector as input and returns a scalar
#' @param na.rm (logical) OPTIONAL: a logical evaluating to `TRUE` or `FALSE`
#'   indicating whether `NA` values should be stripped before computing summary
#'
#' @return (numeric vector) vector of the evaluation of `fn` on rows of `x`
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' summarize_rows(m, min)
#' [1] 1 4 7
#' summarize_rows(m, mean)
#' [1] 2 5 8
# summarize_rows will go row by row in a matrix
# fn is a given operation that we want to have performed on the row
# na.rm = FALSE means that NA will cause the operation to return NA
summarize_rows <- function(x, fn, na.rm=FALSE) {
  # apply will help us apply this to every row in the matrix
  # na.rm = na.rm helps us apply the function within a function to every needed element
  apply(x, 1, function(row) fn(row, na.rm = na.rm))
}

#' Summarize matrix rows into data frame
#'
#' Summarizes the rows of matrix `x`. Returns a data frame with the following
#' columns in order and the corresponding value for each row:
#' 
#'   * mean - arithmetic mean
#'   * stdev - standard deviation
#'   * median - median
#'   * min - minimum value
#'   * max - maximum value
#'   * num_lt_0 - the number of values less than 0
#'   * num_btw_1_and_5 - the number of values between 1 and 5
#'   * num_na - the number of missing (NA) values (ignoring value of na.rm)
#'
#' @param x (numeric matrix): matrix to evaluate the function along rows
#' @param na.rm (logical) OPTIONAL: a logical evaluating to `TRUE` or `FALSE`
#'   indicating whether `NA` values should be stripped before computing summary
#'
#' @return (data frame) data frame containing summarized values for each row of `x`
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' summarize_matrix(m)
#'   mean stdev median min max num_lt_0 num_btw_1_and_5 num_na
#' 1    2     1      2   1   3        0               3      0
#' 2    5     1      5   4   6        0               1      0
#' 3    8     1      8   7   9        0               0      0
#'
#' m <- matrix(rnorm(1000), nrow=4, byrow=T)
#' summarize_matrix(m)
#'          mean    stdev      median       min      max num_lt_0 num_btw_1_and_5 num_na
#' 1 -0.02220010 1.006901  0.02804177 -3.485147 3.221089      120              61      0
#' 2 -0.01574033 1.026951 -0.04725656 -2.967057 2.571608      112              70      0
#' 3 -0.09040182 1.027559 -0.02774705 -3.026888 2.353087      130              54      0
#' 4  0.09518138 1.030461  0.11294781 -3.409049 2.544992       90              72      0
summarize_matrix <- function(x, na.rm=FALSE) {
  # we will create a function within a function
  # summarize_the_row will perform the necessary operations on each row in the matrix x
  # some of the functions are already built in!
  summarize_the_row <- function(row) {
    means <- mean(row, na.rm = na.rm)
    stdevs <- sd(row, na.rm = na.rm)
    medians <- median(row, na.rm = na.rm)
    mins <- min(row, na.rm = na.rm)
    maxes <- max(row, na.rm = na.rm)
    # the other functions are a bit more complicated
    num_lt_0 <- sum(row < 0, na.rm = TRUE)
    num_btw_1_and_5 <- sum(row > 1 & row < 5, na.rm = TRUE)
    num_na <- sum(is.na(row))
    # we want summarize_the_row to return each of the outputs per row
    return(c(mean = means,
             stdev = stdevs,
             median = medians,
             min = mins,
             max = maxes,
             num_lt_0 = num_lt_0,
             num_btw_1_and_5 = num_btw_1_and_5,
             num_na = num_na))
  }
  
  # now we apply this to every row in the matrix
  sum_per_row <- t(apply(x, 1, summarize_the_row))
  # convert it to a data frame
  data_frame_sum <- as.data.frame(sum_per_row)
  # return the data frame!
  return(data_frame_sum)
}

# ------------ Helper Functions Used By Assignment, You May Ignore ------------
#sample_normal <- function(n, mean=0, sd=1) {
#    return(NULL)
#}
#
#sample_normal_w_missing <- function(n, mean=0, sd=1, missing_frac=0.1) {
#    return(NULL)
#}
#
#simulate_gene_expression <- function(num_samples, num_genes) {
#    return(NULL)
#}
#
#simulate_gene_expression_w_missing <- function(num_samples, num_genes, missing_frac=0.1) {
#    return(NULL)
#}

