crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  crossunder_signals <- rep(FALSE, length(arr1))
  if (length(arr1) > 1) for (i in 2:length(arr1)) {
    if (!anyNA(c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1])))
      crossunder_signals[i] <- arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]
  }
  return(crossunder_signals)
}
