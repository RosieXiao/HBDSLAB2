#' Function 2
#'
#'This function is for HBDS5018 Lab2, for calculating statistics over all of the DRG codes for average Medicare payments.
#'
#' @param data a dataset
#' @param stat_type mean,median or sd
#'
#' @return A specific calculation
#' @export
#'
#' @ImportFrom dpryl
#' @ImportFrom tidyverse
#'
#' @examples
#' stats_over_DRG(DRG,"median")
#'
stats_over_DRG <- function(data, stat_type){

  if (stat_type == "mean"){
    mean <- round(mean(data$`Average Medicare Payments`), 3)
    print(paste("The mean of the average medicare payments is", mean))
  } else if (stat_type == "median"){
    median <- round(median(data$`Average Medicare Payments`), 3)
    print(paste("The median of the average medicare payments is", median))
  } else if (stat_type == "sd"){
    sd <- round(sd(data$`Average Medicare Payments`), 2)
    print(paste("The standard deviation of the average medicare payments is", sd))
  } else print("Invalid stat_type. Choose from 'mean', 'median', or 'sd'.")
}
