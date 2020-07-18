#' Groups columns together according to type.
#'
#' This function goes to through the whole dataframe and groups columns with same
#' data types together. User has the option to convert the columns into factors
#' according to the number of unique values present in the column.
#'
#' @usage transform.Group_and_factor(mtcars, convertToFactorFlag = FALSE, conversionThreshold = 5)
#'
#' @param dataf a (non-empty) DataFrame.
#' @param convertToFactorFlag an optional (non-empty) boolean value which allows the user
#' to convert the type of column to factor.
#' @param conversionThreshold an optional (non-empty) integer which acts as a threshold
#' while converting type of column to factor.
#'
#' @return transformed Dataframe
#' @export
#'
#' @examples ## To transform the dataframe to group columns according to data type.
#' transform.Group_and_factor(mtcars)
#'
#' ## To convert columns into factors when unique values are less than a specified number.
#' transform.Group_and_factor(mtcars, convertToFactorFlag = TRUE, conversionThreshold = 3)
transform.Group_and_factor <- function(dataf, convertToFactorFlag = FALSE, conversionThreshold = 5) {
     size <- ncol(dataf)

     if (convertToFactorFlag) {
          for (i in 1:size) {
               number_of_uniques <- length(unique(dataf[, i]))
               if (conversionThreshold >= number_of_uniques)
                    dataf[, i] <- as.factor(dataf[, i])
          }
     }

     finalDF <- dataf[,names(sort(sapply(dataf,class)))]

     return(finalDF)
}
