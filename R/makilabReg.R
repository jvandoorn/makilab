#' Creates and exports a table of LM models with multiple DVs and IVs.
#'
#' @param m1 An object output from lm() or a list of such objects.
#' @param ... More models from lm().
#' @param excel_export A bool for whether not you want an excel export.
#' @param filename String of the filename to export to. Defaults to 'Data_YYYY-MM-DD.xlsx' where YYYY-MM-DD is today's date.
#' @return The table of model summaries and (optional) an excel sheet added to the file.
#' @examples
#' data(iris)
#' library(makilab)
#' m1 <- lm(Petal.Length~Petal.Width+Species, data = iris)
#' m2 <- lm(Sepal.Length~Petal.Width+Sepal.Width, data = iris)
#' m3 <- lm(Petal.Width~Petal.Length+Sepal.Length+Species, data = iris)
#' makilabReg(m1, m2, m3, excel_export = TRUE, filename = "data.xlsx")
#' @export

makilabReg <- function(m1, ... , excel_export=FALSE, filename=NULL){
  models <- list(m1, ...)
  listed <- FALSE
  if (length(models) < 1)
    stop("Not enough arguments.")
  for (i in 1:length(models)) {
    if (!is(models[[i]], "lm") && !is(models[[i]], "list"))
      stop("You must provide lm objects.")
    if (is(models[[i]], "list")) {
      model.list <- models[[i]]
      listed <- TRUE
      for (j in 1:length(model.list)){
        if (!is(model.list[[j]], "lm"))
          stop("Your list must be only of lm objects.")
      }
    }
  }

  ## Unnest lists
  if (listed) {
    if (length(models) == 1) {
      models <- models[[1]]
    } else {
      i <- 0
      while (i < length(models)) {
        i <- i + 1
        if (!is(models[[i]], "lm")) {
          model.list <- models[[i]]
          models <- c(models[i-1], model.list, models[(i+1):length(models)])
        }
      }
    }
  }

  ## Initialize
  columns <- c("pred", "b", "p", "SE", "Model")
  lm.df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  names(lm.df) <- columns

  ## Get variables
  for(i in 1:length(models)){
    vars <- all.vars(models[[i]]$terms)
    model.form <- deparse(formula(models[[i]]$terms))
    yy <- rep("", length(vars)-1)
    b <- rep(0, length(vars)-1)
    p <- b
    SE <- b
    for(j in 1:(length(vars)-1)){
      yy[j] = vars[j+1]
      p[j] = summary(models[[i]])$coefficient[j+1,4]
      b[j] = summary(models[[i]])$coefficient[j+1,1]
      SE[j] = summary(models[[i]])$coefficient[j+1,2]
    }
    new.rows <- data.frame(pred = yy, b=b, p=p, SE=SE, Model = vars[1])
    lm.df <- rbind(lm.df, new.rows)
  }

  ## Arrange the table
  `%>%` <- magrittr::`%>%`
  lm.tab <- lm.df %>%
    tidyr::pivot_wider(names_from = Model,
                values_from = c(b,p,SE),
                names_glue = "{Model}____{.value}") %>%
    dplyr::select(pred, order(colnames(.)))


  ## Export to Excel
  if (excel_export) {
    if (is.null(filename)) {
      filename <- paste0("Data_", Sys.Date(), ".xlsx")
    }
    if (file.exists(filename)) {
      wb <- openxlsx::loadWorkbook(filename)
      cur.sheetnames <- openxlsx::getSheetNames(filename)
      i <- 1
      while (paste0("LM",i) %in% cur.sheetnames)
        i = i + 1
      sheetname <- paste0("LM",i)
    }
    else {
      wb <- openxlsx::createWorkbook(title = paste0("Data_", Sys.Date()))
      sheetname <- paste0("LM",1)
    }

    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, lm.tab, startRow = 10)

    header <- "Linear Regressions"
    subtitle <- "Notes..."
    openxlsx::writeData(wb, sheetname, header, startRow = 1)
    openxlsx::mergeCells(wb, sheetname, cols = 1:10, rows = 1)
    openxlsx::writeData(wb, sheetname, subtitle, startRow = 2)
    openxlsx::mergeCells(wb, sheetname, cols = 1:10, rows = 2)

    ## Conditional Formatting
    sigStyle <- openxlsx::createStyle(bgFill = "#f2dcdb")
    trendStyle <- openxlsx::createStyle(bgFill = "#ebf1de")

    for(k in 2:ncol(lm.tab)){
      if (!(k %% 3 == 0)) {
        next
      }
      ## Trending
      openxlsx::conditionalFormatting(wb, sheetname, cols = k-1, rows = 11:(nrow(lm.tab)+10),
                            rule = paste0("AND(",openxlsx::int2col(k),"11<=0.1,",openxlsx::int2col(k),"11>0)"), style = trendStyle)
      openxlsx::conditionalFormatting(wb, sheetname, cols = k, rows = 11:(nrow(lm.tab)+10),
                            rule = paste0("AND(",openxlsx::int2col(k),"11<=0.1,",openxlsx::int2col(k),"11>0)"), style = trendStyle)
      openxlsx::conditionalFormatting(wb, sheetname, cols = k+1, rows = 11:(nrow(lm.tab)+10),
                            rule = paste0("AND(",openxlsx::int2col(k),"11<=0.1,",openxlsx::int2col(k),"11>0)"), style = trendStyle)

      ## Significant
      openxlsx::conditionalFormatting(wb, sheetname, cols = k-1, rows = 11:(nrow(lm.tab)+10),
                            rule = paste0("AND(",openxlsx::int2col(k),"11<=0.1,",openxlsx::int2col(k),"11>0)"), style = sigStyle)
      openxlsx::conditionalFormatting(wb, sheetname, cols = k, rows = 11:(nrow(lm.tab)+10),
                            rule = paste0("AND(",openxlsx::int2col(k),"11<=0.1,",openxlsx::int2col(k),"11>0)"), style = sigStyle)
      openxlsx::conditionalFormatting(wb, sheetname, cols = k+1, rows = 11:(nrow(lm.tab)+10),
                            rule = paste0("AND(",openxlsx::int2col(k),"11<=0.1,",openxlsx::int2col(k),"11>0)"), style = sigStyle)
    }

    ## String manipulation
    var.names <- as.list(openxlsx::read.xlsx(wb, sheetname, colNames = FALSE, rows = 10))
    var.split <- stringr::str_split(var.names, "____")
    row2 <- sapply(var.split, `[`, 2)
    row1 <- sapply(var.split, `[`, 1)
    openxlsx::writeData(wb, sheetname, t(row2), startRow = 10, colNames = FALSE)
    for(l in 2:ncol(lm.tab)){
       if (!(l %% 3 == 0)) next
       openxlsx::mergeCells(wb, sheetname, cols = (l-1):(l+1), rows = 9)
    }

    ## Formatting
    openxlsx::writeData(wb, sheetname, t(row1), startRow = 9, colNames = FALSE)
    subStyle <- openxlsx::createStyle(halign = "center", textDecoration = "italic")
    openxlsx::addStyle(wb, sheetname, subStyle, rows = c(2:8,10), cols = 1:max(10, ncol(lm.tab)), gridExpand = TRUE)
    headStyle <- openxlsx::createStyle(textDecoration = "bold", halign = "center")
    openxlsx::addStyle(wb, sheetname, headStyle, rows = c(1, 9), cols = 1:max(10, ncol(lm.tab)), gridExpand = TRUE)
    openxlsx::addStyle(wb, sheetname, headStyle, rows = 11:(nrow(lm.tab)+10), cols = 1, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sheetname, 1, 20)


    ## Save book
    openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
  }

  return(lm.tab)

}
