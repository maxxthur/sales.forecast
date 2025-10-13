#' Title
#'
#' @param x
#' @param exclude
#' @param k
#'
#' @return
#' @export
#'
#' @examples
make_lags <- function(x, exclude, k = 24) {
  # get date
  date <- x$date
  # get the relevant variables
  rel <- x[, names(x)[!names(x) %in% exclude]]

  lapply(rel, function(r) {
    lags <- data.frame(matrix(ncol = k+1, nrow = length(r)))
    for(i in 1:(k+1)) {
      lags[, i] <- dplyr::lag(r, i - 1)
    }
    names(lags) <- paste0("lag_", 0:k)
    lags$date <- date
    lags
  })

}
