# BDA400 Assignment 2 - Preliminary Technical Analysis
# Student: Obada Alaswad
# Uses publicly available historical prices; no personal/customer information.
# Install once: install.packages(c("quantmod", "TTR"))

required_packages <- c("quantmod", "TTR")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install missing packages first: ", paste(missing_packages, collapse = ", "))
}

load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  if (!file.exists(portfolio_file)) stop("Portfolio file not found: ", portfolio_file)
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- unique(symbols[nzchar(symbols) & !startsWith(symbols, "#")])
  if (!length(symbols)) stop("No stock symbols found in portfolio.txt")

  loaded <- list()
  for (symbol in symbols) {
    raw <- tryCatch(
      quantmod::getSymbols(symbol, src = "yahoo", from = from, to = to,
                           auto.assign = FALSE, warnings = FALSE),
      error = function(e) {
        warning("Could not download ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(raw) || NROW(raw) == 0L) next
    fields <- data.frame(
      Date = as.Date(zoo::index(raw)),
      Open = as.numeric(quantmod::Op(raw)),
      High = as.numeric(quantmod::Hi(raw)),
      Low = as.numeric(quantmod::Lo(raw)),
      Close = as.numeric(quantmod::Cl(raw)),
      Volume = as.numeric(quantmod::Vo(raw)),
      stringsAsFactors = FALSE
    )
    if ("Adjusted" %in% sub("^.*\\.", "", colnames(raw))) {
      fields$Adjusted <- as.numeric(quantmod::Ad(raw))
    }
    loaded[[symbol]] <- fields[order(fields$Date), , drop = FALSE]
  }
  if (!length(loaded)) stop("No symbols downloaded; check network access and Yahoo symbols")
  loaded
}

# For continuous stock prices, a mode may not exist: return NA when no value repeats.
numeric_mode <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  tab <- table(x)
  if (max(tab) <= 1L) return(NA_real_)
  as.numeric(names(tab)[which.max(tab)])
}

calculate_statistics <- function(stock_df, window = 20L) {
  stopifnot(is.data.frame(stock_df), "Close" %in% names(stock_df), window > 0L)
  close <- as.numeric(stock_df$Close)
  valid <- close[is.finite(close)]
  if (!length(valid)) stop("No usable closing prices")
  sma <- TTR::SMA(close, n = window)
  list(
    summary = data.frame(
      observations = length(valid),
      mean = mean(valid),
      mode = numeric_mode(valid),
      median = median(valid),
      standard_deviation = if (length(valid) > 1) sd(valid) else NA_real_,
      latest_close = tail(valid, 1),
      latest_SMA20 = if (all(is.na(sma))) NA_real_ else tail(na.omit(sma), 1)
    ),
    moving_average = data.frame(Date = stock_df$Date, Close = close,
                                SMA20 = as.numeric(sma))
  )
}

display_stock_data <- function(stocks, n = 6L) {
  for (symbol in names(stocks)) {
    cat("\n===", symbol, "| Last", n, "records ===\n")
    print(utils::tail(stocks[[symbol]], n))
  }
}

display_statistics <- function(statistics) {
  for (symbol in names(statistics)) {
    cat("\n=== Statistics:", symbol, "===\n")
    print(statistics[[symbol]]$summary, row.names = FALSE)
  }
}

plot_stock_analysis <- function(symbol, statistics) {
  d <- statistics[[symbol]]$moving_average
  if (is.null(d)) stop("Symbol not found in statistics list")
  plot(d$Date, d$Close, type = "l", col = "steelblue", lwd = 2,
       main = paste(symbol, "Closing Price and 20-Day SMA"),
       xlab = "Date", ylab = "Price (quote currency)")
  lines(d$Date, d$SMA20, col = "firebrick", lwd = 2)
  legend("topleft", legend = c("Daily close", "20-day SMA"),
         col = c("steelblue", "firebrick"), lty = 1, lwd = 2, bty = "n")
}

run_analysis <- function() {
  stocks <- load_stock_data()
  statistics <- lapply(stocks, calculate_statistics)
  display_stock_data(stocks)
  display_statistics(statistics)
  dir.create("figures", showWarnings = FALSE)
  for (symbol in names(stocks)) {
    filename <- file.path("figures", paste0(gsub("[^A-Za-z0-9_-]", "_", symbol), "_SMA.png"))
    grDevices::png(filename, width = 1000, height = 650, res = 120)
    tryCatch(plot_stock_analysis(symbol, statistics), finally = grDevices::dev.off())
  }
  invisible(list(stocks = stocks, statistics = statistics))
}

# In RStudio, set the working directory to Assignment02 first, then run:
# results <- run_analysis()
