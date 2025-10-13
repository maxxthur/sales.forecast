#' Title
#'
#' @param x a data.frame.
#'
#' @return a vector with the classes of the columns of x.
#' @export
#'
#' @examples
col_class <- function(x) {
  if(!is.data.frame(x)) stop("x must be a data.frame")
  colclass <- sapply(x, class)
  names(colclass) <- NULL
  colclass
}
