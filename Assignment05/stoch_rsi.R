stoch_rsi <- function(data, period, k_period, d_period) {
  if (any(c(k_period, d_period) < 1) || any(c(k_period, d_period) != as.integer(c(k_period, d_period)))) stop("Periods must be positive integers")
  rsi_values <- rsi(data, period)
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) < k_period + d_period - 1) stop("Not enough RSI values for K and D periods")
  # Rolling StochRSI, aligned to source observations. Flat RSI windows are set to zero.
  raw_k <- rep(NA_real_, length(data))
  for (i in (period + k_period):length(data)) {
    window <- rsi_values[(i - k_period + 1):i]
    low <- min(window); high <- max(window)
    raw_k[i] <- if (high == low) 0 else (rsi_values[i] - low) / (high - low)
  }
  # k_period is the RSI normalization window; d_period smooths that K line.
  k_line <- raw_k
  d_line <- rep(NA_real_, length(data))
  start <- period + k_period + d_period - 1
  if (start <= length(data)) for (i in start:length(data)) d_line[i] <- sum(k_line[(i - d_period + 1):i]) / d_period
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
