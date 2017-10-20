#' monthIndex
#'
#' A function to create a month index variable
#'
#' @param x A vector or column of dates
#' @keywords internal
monthIndex <- function(x){
  month <- as.numeric(format(x,"%m"))+
    (as.numeric(format(x,"%Y"))-
       as.numeric(format(x[1],"%Y")))*12-
    as.numeric(format(x[1],"%m"))
  return(month)
}

#' quarterIndex
#'
#' A function to create a quarter index variable
#'
#' @param x A vector or column of dates
#' @keywords internal
quarterIndex <- function(x){
  quarter <- ceiling(as.numeric(format(x,"%m"))/3)+
    (as.numeric(format(x,"%Y"))-
       as.numeric(format(x[1],"%Y")))*4-2
  quarter <- quarter - (quarter[1]-1)
  return(quarter)
}

#' geomean
#'
#' function for the geometric mean of a vector
#' @param x a numeric vector
#' @keywords internal
geomean <- function(x){
  return(exp(mean(log(x))))
}
