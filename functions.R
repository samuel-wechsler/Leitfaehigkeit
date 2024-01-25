#' Calculate mean and confidence interval
#'
#' This function takes a numeric vector as input and computes the mean
#' along with the confidence interval using a t-distribution.
#'
#' @param data A numeric vector for which the mean and confidence interval
#' need to be calculated.
#'
#' @return A list with two elements: mean and confidence interval.
#' @seealso \code{\link{qt}}
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' result <- calculate_mean_ci(data)
#' result$mean # contains the mean value
#' result$ci # contains the confidence interval
calculate_mean_ci <- function(data) {
  mean_value <- mean(data)
  sd_value <- sd(data)
  n <- length(data)
  se <- sd_value / sqrt(n)
  
  t_s = qt(0.975, df = n - 1)
  
  ci <- t_s * se  # Using the t-distribution multiplier
  
  return(list(mean = mean_value, ci = ci))
}


#' Format the mean and confidence interval
#'
#' This function takes a list containing mean and confidence interval values
#' and formats them into a string for easy display.
#'
#' @param mean_ci A list with mean and confidence interval values,
#' typically the output of \code{\link{calculate_mean_ci}}.
#'
#' @param r The number of decimal places to round the mean and confidence interval values.
#'
#' @return A character string representing the formatted mean and
#' confidence interval.
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' result <- calculate_mean_ci(data)
#' formatted_result <- format_mean_ci(result, 3)
#' formatted_result # contains the formatted string for mean and confidence interval
format_mean_ci <- function(mean_ci, r) {
  return(paste(round(mean_ci$mean, r), "\u00B1", round(mean_ci$ci, r)))
}

