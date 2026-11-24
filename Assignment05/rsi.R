rsi <- function(data, period) {
  if (length(period) != 1 || is.na(period) || period < 1 || period != as.integer(period)) stop("Invalid period")
  n <- length(data)
  if (n <= period) stop("Data must have at least period + 1 values")
  differences <- diff(data)
  gains <- pmax(differences, 0)
  losses <- pmax(-differences, 0)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, n)
  calc_rsi <- function(g, l) if (g == 0 && l == 0) 50 else if (l == 0) 100 else 100 - 100 / (1 + g / l)
  rsi_values[period + 1] <- calc_rsi(avg_gain, avg_loss)
  if (n > period + 1) for (i in (period + 2):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    rsi_values[i] <- calc_rsi(avg_gain, avg_loss)
  }
  return(rsi_values)
}
