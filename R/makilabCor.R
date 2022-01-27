#' Creates and exports a correlation matrix in a style consistent with the Maki Lab.
#'
#' @param df A data frame containing the variables you want to correlate.
#' @param x A list of column names for the X axis.
#' @param y A list of row names for the Y axis.
#' @param excel_export A bool for whether or not you want an excel export.
#' @param filename String of the filename to export to. Defaults to 'Data_YYYY-MM-DD.xlsx' where YYYY-MM-DD is today's date.
#' @return The table of correlation values.
#' @examples
#' data(iris)
#' library(makilab)
#' makilabCor(df = iris,
#'            x = c("Sepal.Width", "Sepal.Length"),
#'            y = c("Petal.Width", "Petal.Length", "Species")) # No export
#' makilabCor(df = iris,
#'            x = c("Sepal.Width", "Sepal.Length"),
#'            y = c("Petal.Width", "Petal.Length", "Species"),
#'            excel_export = TRUE) # Exports to excel
#' @export
makilabCor <- function(df,x,y,excel_export=FALSE,filename=NULL){
  if(!is.data.frame(df))
    stop("You must provide a data frame.")
  if(!is.character(x) | !is.character(y))
    stop("You must give the variables as a list of characters.")

  ## Make character groups correlatable
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)

  ## Initialize
  columns <- c("X", "Y", "corr", "p")
  corr.df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  names(corr.df) <- columns

  ## Run correlations
  for(i in 1:length(x)){
    corr <- rep(0, length(y))
    pval <- corr
    yy <- rep("", length(y))
    for(j in 1:length(y)){
      test <- cor.test(as.numeric(df[[x[i]]]),as.numeric(df[[y[j]]]))
      corr[j] = test$estimate
      pval[j] = test$p.value
      yy[j] = y[j]
    }
    new.rows <- data.frame(X = x[i], Y=yy, corr=corr, p=pval)
    corr.df <- rbind(corr.df, new.rows)
  }

  ## Arrange the table
  `%>%` <- magrittr::`%>%`
  corr.tab <- corr.df %>%
    tidyr::pivot_wider(names_from = X,
                values_from = c(corr,p),
                names_glue = "{X}_{.value}") %>%
    dplyr::select(Y, order(colnames(.)))


  ## Export to Excel
  if (excel_export) {
    if (is.null(filename)) {
      filename <- paste0("Data_", Sys.Date(), ".xlsx")
    }
    if (file.exists(filename)) {
      wb <- openxlsx::loadWorkbook(filename)
      cur.sheetnames <- openxlsx::getSheetNames(filename)
      i <- 1
      while (paste0("Corr",i) %in% cur.sheetnames)
        i = i + 1
      sheetname <- paste0("Corr",i)
    }
    else {
      wb <- openxlsx::createWorkbook(title = paste0("Data_", Sys.Date()))
      sheetname <- paste0("Corr",1)
    }

    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, corr.tab, startRow = 5)

    header <- "Pearson's Correlation of..."
    subtitle <- "Notes..."
    openxlsx::writeData(wb, sheetname, header, startRow = 1)
    openxlsx::mergeCells(wb, sheetname, cols = 1:10, rows = 1)
    openxlsx::writeData(wb, sheetname, subtitle, startRow = 2)
    openxlsx::mergeCells(wb, sheetname, cols = 1:10, rows = 2)

    ## Conditional Formatting
    sigStyle <- openxlsx::createStyle(bgFill = "#f2dcdb")
    trendStyle <- openxlsx::createStyle(bgFill = "#ebf1de")

    for(k in 2:ncol(corr.tab)){
      if (k %% 2 == 1) {
        next
      }
      openxlsx::conditionalFormatting(wb, sheetname, cols = k, rows = 6:(nrow(corr.tab)+5),
                            rule = paste0(openxlsx::int2col(k+1),"6<=0.1"), style = trendStyle)
      openxlsx::conditionalFormatting(wb, sheetname, cols = k+1, rows = 6:(nrow(corr.tab)+5),
                            rule = paste0(openxlsx::int2col(k+1),"6<=0.1"), style = trendStyle)
      openxlsx::conditionalFormatting(wb, sheetname, cols = k, rows = 6:(nrow(corr.tab)+5),
                            rule = paste0(openxlsx::int2col(k+1),"6<=0.05"), style = sigStyle)
      openxlsx::conditionalFormatting(wb, sheetname, cols = k+1, rows = 6:(nrow(corr.tab)+5),
                            rule = paste0(openxlsx::int2col(k+1),"6<=0.05"), style = sigStyle)
    }

    ## String manipulation
    var.names <- as.list(openxlsx::read.xlsx(wb, sheetname, colNames = FALSE, rows = 5))
    var.split <- stringr::str_split(var.names, "_")
    row2 <- sapply(var.split, `[`, 2)
    row1 <- sapply(var.split, `[`, 1)
    openxlsx::writeData(wb, sheetname, t(row2), startRow = 5, colNames = FALSE)
    row1[c(TRUE, FALSE)] <- ""
    for(l in 2:ncol(corr.tab)){
      if (l %% 2 == 1) next
      openxlsx::mergeCells(wb, sheetname, cols = l:(l+1), rows = 4)
    }
    openxlsx::writeData(wb, sheetname, t(row1), startRow = 4, colNames = FALSE)
    subStyle <- openxlsx::createStyle(halign = "center", textDecoration = "italic")
    openxlsx::addStyle(wb, sheetname, subStyle, rows = 1:5, cols = 1:max(10, ncol(corr.tab)), gridExpand = TRUE)
    headStyle <- openxlsx::createStyle(textDecoration = "bold", halign = "center")
    openxlsx::addStyle(wb, sheetname, headStyle, rows = c(1, 4), cols = 1:max(10, ncol(corr.tab)), gridExpand = TRUE)
    openxlsx::addStyle(wb, sheetname, headStyle, rows = 5:(nrow(corr.tab)+5), cols = 1, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sheetname, 1, 20)


    ## Save book
    openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
  }

  return(corr.tab)

}
