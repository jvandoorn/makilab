
# makilab

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/makilab)](https://CRAN.R-project.org/package=makilab)
<!-- badges: end -->

The goal of makilab is to provide a collection of helpful functions for the use of projects and testing specific to the lab of Pauline Maki at the Unversity of Illinois, Chicago. Includes useful MS Excel exports, raw data parsing, and cognitive test grading.

## Installation

You can install the development version of makilab like so:

``` r
install.packages("devtools")
devtools::install_github("jvandoorn/makilab")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(makilab)

## Make a Pearson Correlation
data(iris)
makilabCor(df = iris, x = c("Sepal.Width", "Sepal.Length"), y = c("Petal.Width", "Petal.Length", "Species")) # No export
makilabCor(df = iris, x = c("Sepal.Width", "Sepal.Length"), y = c("Petal.Width", "Petal.Length", "Species"), excel_export = TRUE) # Exports to excel in your working directory called Data_todays date.xlsx

## Export a hierarchical LM model to Excel
data(iris)
m1 <- lm(Sepal.Length~Petal.Length, data = iris)
m2 <- lm(Sepal.Length~Petal.Length+Petal.Width, data = iris)
m3 <- lm(Sepal.Length~Petal.Length+Petal.Width+Species, data = iris)
makilabReg(m1, m2, m3, excel_export = TRUE)
```
