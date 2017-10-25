#' geomean
#'
#' function for the geometric mean of a vector
#' @param x a numeric vector
#' @keywords internal
geomean <- function(x){
  return(exp(mean(log(x))))
}
