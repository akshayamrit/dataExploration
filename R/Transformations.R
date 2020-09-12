#' Groups columns together according to type.
#'
#' This function goes to through the whole dataframe and groups columns with same
#' data types together. User has the option to convert the columns into factors
#' according to the number of unique values present in the column.
#'
#' @usage mtcars <- transform.group_and_factor(mtcars, convertToFactorFlag = FALSE, conversionThreshold = 5)
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
#' transform.group_and_factor(mtcars)
#'
#' ## To convert columns into factors when unique values are less than a specified number.
#' transform.group_and_factor(mtcars, convertToFactorFlag = TRUE, conversionThreshold = 3)
transform.group_and_factor <- function(dataf, convertToFactorFlag = FALSE, conversionThreshold = 5) {
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



#' Imputes mean, median or mode in a single column
#'
#' This function imputes the missing data on the column selected by the user,
#' using the statistic selected by the user
#'
#' @usage mtcars <- transform.impute_option(mtcars, 'origin', 'mode')
#'
#' @param dataf a (non-empty) Dataframe
#' @param variable.name a (non-empty) column name with missing values
#' @param option the method to be used. options - 'mean', 'median' and 'mode'
#'
#' @return dataframe with imputed values
#' @export
#'
#' @examples # To get dataframe with mean imputed in 'wt' column.
#' transform.impute_option(mtcars, 'wt', 'mean')
transform.impute_option <- function(dataf, variable.name, option) {
        if (!is.data.frame(dataf)) stop('Please pass a dataframe as parameter')
        Mode <- function(x) {
                ux <- unique(x[!is.na(x)])
                return(ux[which.max(tabulate(match(x, ux)))])
        }

        impute.value <- NULL
        if (option == 'mean') impute.value <- mean(dataf[, variable.name], na.rm = T)
        else if (option == 'median') impute.value <- median(dataf[, variable.name], na.rm = T)
        else if (option == 'mode') impute.value <- Mode(dataf[, variable.name])
        else stop('Select a valid option')
        dataf[is.na(dataf[, variable.name]), variable.name] = impute.value
        return(dataf)
}


#' Augments the process of imputation by providing relevant visual cues to the user.
#'
#' This function provides the user relevant information about each variable defined
#' by the user iteratively and lets the user decide the kind of imputation they would
#' like to make on it. In case the variable is numeric, summary with boxplot and histogram
#' are displayed whereas in case of catogorical variables, table with bar graph and
#' pie chart are displayed. User gets the option to impute using 'mean', 'mode' or
#' 'median'.
#'
#' @usage kc_complete <- transform.augment_impute(kc_complete)
#'
#' @param dataf a (non-empty) Dataframe
#' @param variables a list of (non-empty) column names with missing values. all columns
#' are considered by default.
#' @param missingThreshold a (non-empty) number between 0 to 1
#'
#' @return
#' @export
#'
#' @examples
transform.augment_impute <- function(dataf, variables = colnames(dataf), missingThreshold = 0.3) {
        if (!is.data.frame(dataf)) stop('Please pass a dataframe as parameter')
        for (i in variables) {
                all.cols <- colnames(dataf)

                if (!(i %in% all.cols)) stop('one or more variable not available in the dataframe')
        }

        dataf <- as.data.frame(dataf)


        for (i in variables) {
                missing.prop <- sum(is.na(dataf[, i]))/nrow(dataf)
                if (missing.prop > 0) {
                        print(paste('Proportion of missing values:', missing.prop))

                        if (is.numeric(dataf[, i])) {
                                print(summary(dataf[, i]))
                                par(mfrow=c(2,1))
                                boxplot(dataf[,i], main = paste("Boxplot of", i),
                                        ylab = i, col = "maroon", border = "grey5", horizontal = T)

                                hist(dataf[,i], main = paste("Histogram of", i),
                                     xlab = i, ylab = "No. of Houses", col = "orange", border=F)
                        }
                        else if (is.factor(dataf[, i]) || is.character(dataf[, i])) {
                                par(mfrow = c(1,2))
                                cat.table <- table(dataf[,i])
                                print(cat.table)
                                barplot(cat.table, main = paste("Barplot of", i),
                                        xlab = i, ylab = 'Frequency', col = "orange")

                                pie(cat.table, labels = round(cat.table/sum(cat.table)*100, 1), col = rainbow(length(cat.table)),
                                    main = paste('Pie chart of', i))
                                legend('topright', names(cat.table), fill = rainbow(length(cat.table)))
                        }
                        else
                                stop('This datatype is not supported. Please contact the function creator to fix it.')

                        option <- 0
                        if (missing.prop > missingThreshold) {
                                print(paste('The proportion of missing value in', i, 'exceeds the threshold.'))
                                option <- readline('Select \'1\' to drop this column. To cancel, select \'0\':')
                                if (option == 1) {
                                        curr.col <- grep(i, colnames(dataf))
                                        dataf <- dataf[, -curr.col]
                                        print('Column Deleted')
                                }
                                else{
                                        print('Column not deleted')
                                }

                        }

                        if (option == 0) {
                                if (is.numeric(dataf[, i])) {
                                        option = readline(paste('Press 1 to impute column', i, 'with mean. Select 2 to impute column', i, 'with median. Press 0 to skip:'))
                                        if (option == 1)
                                                dataf <- impute_option(dataf, i, 'mean')
                                        else if (option == 2)
                                                dataf <- impute_option(dataf, i, 'median')
                                        else if (option == 0)
                                                print('Imputation cancelled')
                                        else
                                                print('Wrong or Invalid option selected. Skipping imputation for this column')
                                }
                                else if (is.factor(dataf[, i]) || is.character(dataf[, i])) {
                                        option = readline(paste('Press 1 to impute column', i, 'with mode. Select 2 to impute column', i, 'with median. Press 0 to skip:'))

                                        if (option == 1)
                                                dataf <- impute_option(dataf, i, 'mode')
                                        else if (option == 2)
                                                dataf <- impute_option(dataf, i, 'median')
                                        else if (option == 0)
                                                print('Imputation cancelled')
                                        else
                                                print('Wrong or Invalid option selected. Skipping imputation for this column')

                                }
                        }

                }
        }
        return(dataf)

}
