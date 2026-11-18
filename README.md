# BDA400 Assignment 2 - Preliminary Technical Analysis

This folder contains a reproducible introductory stock-market technical analysis using R.

## Files

- `portfolio.txt`: one Yahoo Finance stock symbol per line.
- `technical_analysis.R`: imports OHLCV data, calculates summary statistics and moving averages, prints results, and saves price charts.
- `Alaswad_Obada_CA_BDA400_A02.docx`: report with setup guidance and evidence placeholders.

## Run

1. Install R and RStudio Desktop.
2. Install packages: `install.packages(c("quantmod", "TTR"))`.
3. Open this assignment folder in RStudio and set it as the working directory.
4. Run:

```r
source("technical_analysis.R")
results <- run_analysis()
```

Internet access is required to fetch current Yahoo Finance price histories. Individual unavailable symbols are skipped with a warning. The `figures` folder is created by the script after it runs.

**Important:** The mode is reported as `NA` when each closing price is unique. Prices and outputs depend on the run date and on the data provider. No financial trading recommendation is implied.
