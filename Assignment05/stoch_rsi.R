
stoch_rsi <- function(data, period, k_period, d_period) {

  # Calculate RSI
  rsi_values <- rsi(data, period)

  # Remove unavailable initial RSI values
  valid_rsi <- rsi_values[!is.na(rsi_values)]

  if (length(valid_rsi) < k_period + d_period - 1) {
    stop("Not enough data for Stochastic RSI")
  }

  # Calculate normalized RSI values
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)

  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(valid_rsi))
  } else {
    k_values <- (valid_rsi - min_rsi) /
                (max_rsi - min_rsi)
  }

  # Calculate K using Simple Moving Average
  k_smoothed <- sma(k_values, k_period)

  # Calculate D using Simple Moving Average
  d_smoothed <- sma(k_smoothed, d_period)

  # Align results with the original data
  k_line <- rep(NA_real_, length(data))
  d_line <- rep(NA_real_, length(data))

  k_start <- period + k_period
  d_start <- k_start + d_period - 1

  k_line[k_start:length(data)] <- k_smoothed
  d_line[d_start:length(data)] <- d_smoothed

  # Return results
  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}
