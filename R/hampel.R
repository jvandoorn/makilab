#' Applies a Hampel Filter to the provided data.
#'
#' The Hampel filter is an outlier detection function used in time series analysis
#' that finds the median value of the input vector and the mean absolute deviation
#' (MAD). Outliers are determined by being more than n MADs from the
#' median. Typically this is applied as a sliding window.
#'
#' @param x A vector of numerical data.
#' @param n The number of MADs from the median to limit the data. Default is 3.
#' @param k A scaling factor of the MAD. Default is for Gaussian data.
#' @param replace A boolean to determine if outliers should be replaced by the median. If set to FALSE, outliers will be set to NA. Default is FALSE.
#' @return The new vector with outliers replaced and a list of indices where the outliers were.
#' @examples
#' data(iris)
#' library(makilab)
#' hampal(Sepal.Length)
#' @export
hampel <- function(x, n = 3, k = 1.4826, replace = FALSE) {
  y <- x # Output vector

  m <- median(x, na.rm = TRUE)
  md <- mad(x, constant = k, na.rm = TRUE)

  x_ind <- which(x >= m + n * md | x <= m - n * md)

  if (replace) {
    y[x_ind] <- m
  } else {
    y[x_ind] <- NA
  }

  list(y = y, ind = x_ind)
}
