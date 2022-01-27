
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

This is a basic example which shows you how to use the Excel export functions:

``` r
library(makilab)

## Make a Pearson Correlation
data(iris)
makilabCor(df = iris, x = c("Sepal.Width", "Sepal.Length"), y = c("Petal.Width", "Petal.Length", "Species")) # No export
makilabCor(df = iris, x = c("Sepal.Width", "Sepal.Length"), y = c("Petal.Width", "Petal.Length", "Species"), excel_export = TRUE) # Exports to excel in your working directory called Data_todays date.xlsx

## Export a hierarchical LM model to Excel
data(iris)
m1 <- lm(Petal.Length~Petal.Width, data = iris)
m2 <- lm(Petal.Length~Petal.Width+Sepal.Length, data = iris)
m3 <- lm(Petal.Length~Petal.Width+Sepal.Length+Species, data = iris)

makilabHLM(m1,m2,m3, excel_export = TRUE, filename = "makilabTest.xlsx")

mlist <- list(m1,m2,m3)
makilabHLM(mlist, excel_export = TRUE, filename = "makilabTest.xlsx")

mlist <- list(m1,m2)
makilabHLM(mlist,m3, excel_export = TRUE, filename = "makilabTest.xlsx")

## Export a non-hierarchical set of LM models to Excel
data(iris)
m1 <- lm(Petal.Length~Petal.Width+Species, data = iris)
m2 <- lm(Sepal.Length~Petal.Width+Sepal.Width, data = iris)
m3 <- lm(Petal.Width~Petal.Length+Sepal.Length+Species, data = iris)

makilabReg(m1,m2,m3, excel_export = TRUE, filename = "makilabTest.xlsx")

mlist <- list(m1,m2,m3)
makilabReg(mlist, excel_export = TRUE, filename = "makilabTest.xlsx")

mlist <- list(m1,m2)
makilabReg(mlist,m3, excel_export = TRUE, filename = "makilabTest.xlsx")
```
