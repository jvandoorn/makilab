% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hampel.R
\name{hampel}
\alias{hampel}
\title{Applies a Hampel Filter to the provided data.}
\usage{
hampel(x, n = 3, k = 1.4826, replace = FALSE)
}
\arguments{
\item{x}{A vector of numerical data.}

\item{n}{The number of MADs from the median to limit the data. Default is 3.}

\item{k}{A scaling factor of the MAD. Default is for Gaussian data.}

\item{replace}{A boolean to determine if outliers should be replaced by the median. If set to FALSE, outliers will be set to NA. Default is FALSE.}
}
\value{
The new vector with outliers replaced and a list of indices where the outliers were.
}
\description{
The Hampel filter is an outlier detection function used in time series analysis
that finds the median value of the input vector and the mean absolute deviation
(MAD). Outliers are determined by being more than n MADs from the
median. Typically this is applied as a sliding window.
}
\examples{
data(iris)
library(makilab)
iris$Sepal.Length[20] <- 99 # creating an outlier
hampal(iris$Sepal.Length)
}
