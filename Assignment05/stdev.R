stdev <- function(data) {
  if (!length(data)) stop("Data cannot be empty")
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values^2
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
