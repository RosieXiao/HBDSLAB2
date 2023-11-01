#' Boxplot for function 1
#'
#' @param data the dataset
#' @param grouping_column the grouping variable
#' @param payment_column payment type
#'
#' @return A boxplot between DRG Code(Grouping variable) and payment type
#' @export
#'
#'@importFrom ggplot2 ggplot
#'@importFrom dpryl
#'
#' @examples
#' boxplot2(DRG,'DRG.Definition', 'Average.Covered.Charges')
#'
boxplot2 <- function(data, grouping_column, payment_column) {

  if (!all(c(grouping_column, payment_column) %in% colnames(data))) {
    stop(paste("Column(s) missing from data:",
               paste(setdiff(c(grouping_column, payment_column), colnames(data)), collapse=", ")))
  }

  data$short_grouping <- substr(data[[grouping_column]], 1, 3)


  boxplot(data[[payment_column]] ~ data$short_grouping,
          main=paste0("Boxplot of ", payment_column, " by ", grouping_column),
          xlab='DRG Code',
          ylab=payment_column,
          las=2,
          cex.axis=0.5)
}
