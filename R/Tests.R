#' Pearson's Chi-squared Test. (Category vs Category)
#'
#' This function performs chi square test between the categorical target variable
#' and user defined columns of the same dataset.
#'     Note: Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#'
#' @usage test.Chisq(dataf, y, x, skipMessage = FALSE)
#'
#' @param dataf a (non-empty) dataframe.
#' @param y a (non-empty) integer which points to the column which will be considered
#' as the target variable.
#' @param x a (non-empty) vector of integers which points to the columns which should
#' be used for the test.
#' @param skipMessage an optional (non-empty) boolean which lets the user choose whether
#' he should get the warning messages.
#'
#' @return dataframe containing the result of the tests.
#' @export
#'
#' @examples test.Chisq(mtcars, 1, 2:ncol(mtcars))
test.Chisq <- function(dataf, y, x, skipMessage = FALSE) {

     if (!is.data.frame(dataf))
          stop("The given object is not a data frame")
     if (!is.factor(dataf[,y]) && !is.character(dataf[, y]))
          stop('Target variable is not a categorical variable')
     if (y %in% x)
          stop('Target is present in list of variables as well')

     row_data <- list()
     var.name <- c()
     stat <- c()
     p.value <- c()
     significance <- c()
     counter <- 1
     for (i in x) {
          if (is.factor(dataf[, i]) || is.character(dataf[, i])) {
               chi.result <- suppressWarnings(chisq.test(dataf[,i], dataf[, y]))
               var.name[counter] <- names(dataf)[i]
               stat[counter] <- chi.result$statistic
               p.value[counter] <- chi.result$p.value
               significance[counter] <- ifelse(chi.result$p.value >= 0.1, ' ',
                                               ifelse(chi.result$p.value >= 0.05, '.',
                                                      ifelse(chi.result$p.value >= 0.01, '*',
                                                             ifelse(chi.result$p.value >= 0.001, '**',
                                                                    '***'))))
               counter = counter + 1
          }
          else
               if (!skipMessage)
                    print(paste(names(dataf)[i], 'will be skipped as it is not a categorical variable'))
     }
     row_data$var_name <- var.name
     row_data$x_square <- stat
     row_data$p_value <- p.value
     row_data$significance <- significance
     finalDF <- as.data.frame(row_data, row.names = row_data$var_name)
     return(finalDF)
}



#' Student's t-Test. (Category for 2 values vs Numeric)
#'
#' This function performs t test between the categorical target (upto 2 categories)
#' and user defined columns of the same dataset.
#'     Note: Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#'
#' @usage test.T_test.Cat(dataf, y, x, skipMessage = FALSE)
#'
#' @param dataf a (non-empty) dataframe.
#' @param y a (non-empty) integer which points to the column which will be considered
#' as the target variable.
#' @param x a (non-empty) vector of integers which points to the columns which should
#' be used for the test.
#' @param skipMessage an optional (non-empty) boolean which lets the user choose whether
#' he should get the warning messages.
#'
#' @return dataframe containing the result of the tests.
#' @export
#'
#' @examples test.T_test.Cat(mtcars, 2, 3:ncol(mtcars))
test.T_test.Cat <- function(dataf, y, x, skipMessage = FALSE) {

     if (!is.data.frame(dataf))
          stop("The given object is not a data frame")
     if (!is.factor(dataf[,y]) && !is.character(dataf[, y]))
          stop('Target variable is not a categorical variable')
     if (length(unique(dataf[, y])) != 2)
          stop('grouping factor must have exactly 2 levels')
     if (y %in% x)
          stop('Target is present in list of variables as well')

     row_data <- list()
     var.name <- c()
     stat <- c()
     p.value <- c()
     significance <- c()
     counter <- 1
     for (i in x) {
          if (is.numeric(dataf[, i]) || is.integer(dataf[, i])) {
               tTest <- t.test(dataf[,i]~dataf[, y])
               var.name[counter] <- names(dataf)[i]
               stat[counter] <- tTest$statistic
               p.value[counter] <- tTest$p.value
               significance[counter] <- ifelse(tTest$p.value >= 0.1, ' ',
                                               ifelse(tTest$p.value >= 0.05, '.',
                                                      ifelse(tTest$p.value >= 0.01, '*',
                                                             ifelse(tTest$p.value >= 0.001, '**',
                                                                    '***'))))
               counter = counter + 1
          }
          else
               if (!skipMessage)
                    print(paste(names(dataf)[i], 'will be skipped as it is not a numerical variable'))
     }
     row_data$var_name <- var.name
     row_data$t <- stat
     row_data$p_value <- p.value
     row_data$significance <- significance
     finalDF <- as.data.frame(row_data, row.names = row_data$var_name)
     return(finalDF)
}



#' Perform every test for Categorical Target.
#'
#' This function lets us perform the following tests when the target is categorical:
#' - Chi-Square Test
#' - T-Test (Only when number of categories is 2 in target variable)
#'
#' @usage
#'
#'
#' @param dataf a (non-empty) dataframe.
#' @param y a (non-empty) integer/string which points to the column which will be considered
#' as the target variable.
#' @param x a (non-empty) vector of integers/string which points to the columns which should
#' be used for the test.
#' @param chiSquare an optional (non-empty) variable which lets the user decide whether
#' Chi-Square test should be performed.
#' @param tTest an optional (non-empty) variable which lets the user decide whether
#' T-test should be performed.
#'
#' @return list of dataframes returned from other tests.
#' @export
#'
#' @examples ## To perform every test.
#' test.All.Cat(mtcars, 'vs', 3:ncol(mtcars))
#'
#' ## To omit a specefic test.
#' test.All.Cat(mtcars, 'cyl', 2:ncol(mtcars), tTest = FALSE)
test.All.Cat <- function(dataf, y, x, chiSquare = TRUE, tTest = TRUE) {
     if (!is.data.frame(dataf))
          stop("The given object is not a data frame")
     pred <- c()
     counter <- 1
     if (is.character(y))
          y <- grep(y, colnames(dataf))
     if (is.character(x)) {
          for (i in x) {
               pred[counter] <- grep(i, colnames(dataf))
               counter = counter + 1
          }
     }
     else{
          pred <- x
     }

     resultList <- list()
     if (chiSquare)
          resultList$chiSquare <- test.Chisq(dataf, y, pred, TRUE)
     if (tTest)
          resultList$tTest <- test.T_test.Cat(dataf, y, pred, TRUE)

     return(resultList)
}
