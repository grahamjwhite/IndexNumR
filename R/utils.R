#' geomean
#'
#' function for the geometric mean of a vector
#' @param x a numeric vector
#' @keywords internal
geomean <- function(x){
  return(exp(mean(log(x))))
}

#' checkNames
#'
#' checks if elements of namesVector exist in column names of x
#' @param x a dataframe
#' @param namesVector a vector of strings
#' @return a list where the first element is either TRUE if all
#' strings in namesVector exist in the column names of x,
#' otherwise FALSE. If FALSE, then checkNames returns a second
#' element with an error message that contains a list of the
#' names not contained in the column of x.
#' @keywords internal
checkNames <- function(x, namesVector){
  goodNames <- colnames(x)
  badNames <- namesVector[!(namesVector %in% goodNames)]

  if(length(badNames >= 1)){
    err <- paste("The specified name(s) -", paste(badNames, collapse = ", "),
                  "- are not column names of the input data frame. Check the
                 names given to the pvar, qvar, pervar and prodID arguments.")
    return(list(result=FALSE,message=err))
  }
  else {
    return(list(result=TRUE))
  }
}
