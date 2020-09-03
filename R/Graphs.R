#' Generate Graphs for Univariate Analysis.
#'
#' This function lets the user perform univariate analysis of every column present in
#' the dataframe by default. User can specify the particular rows he wants to create
#' visualizations for by assigning some value to the 'variable' argument. The user can also choose
#' to export these visualizations in png format to the directory specified by the user.
#'
#' @usage graph.univariate(dataf, variable = 1:ncol(dataf), generate.png = FALSE,
#' generated.file.location = paste(getwd(), '/Images/Univariate'))
#'
#' @param dataf a (non-empty) DataFrame.
#' @param variable an optional(non-empty) numeric vector of column numbers.
#' @param generate.png an optional (non-empty) boolean value to specify whether
#' the visualization has to be exported as png or not.
#' @param generated.file.location an optional (non-empty) string which specifies
#' where the exported images needs to be stored.
#'
#' @return NA
#' @export
#'
#' @examples ## To generate visualization of every column.
#' graph.univariate(mtcars)
#'
#' ## To generate visualization of specefic columns.
#' graph.univariate(mtcars, variable = c(1,2,3))
#'
#' ## To generate visualization and store it as image.
#' graph.univariate(mtcars, variable = c(1,2,3), generate.png = TRUE,
#' generated.file.location = paste(getwd(), '/Images/Univariate'))
#'
graph.univariate <- function(dataf, variable = 1:ncol(dataf), generate.png = FALSE,
                             generated.file.location = 'Images/Univariate') {

        if (!is.data.frame(dataf))
                stop("The given object is not a data frame")

        defaultwd <- getwd()
        if (generate.png) {
                dir.create(generated.file.location, showWarnings = FALSE, recursive = T)
                setwd(generated.file.location)
        }

        for (i in variable) {
                if (generate.png)
                        png(paste(names(dataf)[i], ".png", sep=""))

                if (is.numeric(dataf[,i])) {
                        par(mfrow=c(2,1))
                        boxplot(dataf[,i], main = paste("Boxplot of", names(dataf)[i]),
                                ylab = names(dataf)[i], col = "maroon", border = "grey5", horizontal = T)

                        hist(dataf[,i], main = paste("Histogram of", names(dataf)[i]),
                             xlab = names(dataf)[i], ylab = "No. of Houses", col = "orange", border=F)

                }

                else if (is.factor(dataf[, i]) || is.character(dataf[, i])) {
                        par(mfrow = c(1,2))
                        cat.table <- table(dataf[,i])
                        barplot(cat.table, main = paste("Barplot of", names(dataf)[i]),
                                xlab = names(dataf)[i], ylab = 'Frequency', col = "orange")

                        pie(cat.table, labels = round(cat.table/sum(cat.table)*100, 1), col = rainbow(length(cat.table)),
                            main = paste('Pie chart of', names(dataf)[i]))
                        legend('topright', names(cat.table), fill = rainbow(length(cat.table)))
                }

                if (generate.png)
                        dev.off()
        }

        if (generate.png)
                setwd(defaultwd)
}
