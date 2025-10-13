#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_ymd <- function(x) {
  all(grepl(pattern = "\\d{4}-\\d{2}-\\d{2}", x = x))
}
