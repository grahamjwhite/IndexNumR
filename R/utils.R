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

#' isContinuous
#'
#' checks if a numeric vector has gaps.
#' @param x Vector to check
#' @return a list where the first element contains the
#' result of the check and the second element contains
#' the list of missing elements.
#' @keywords internal
isContinuous <- function(x){

  check <- all(min(x):max(x) %in% unique(x))

  if(!check){
    missing <- setdiff(min(x):max(x), unique(x))
    err <- paste("The following elements are missing: ",
                 missing)
    return(list(result = FALSE, missing = missing))
  }
  else {
    return(list(result = TRUE))
  }

}

#' getFrequency
#'
#' get the frequency of a vector of dates
#' @param x a vector of dates
#' @return an integer indicating the number of
#' periods per year.
#' @keywords internal
getFrequency <- function(x){
  return(round(365/((x[3]-x[1])/2)),0)
}

#' print.index
#'
#' a custom print method for the index class
#' @param x index object to print
#' @export
print.index <- function(x){
  s <- paste0("Index","\n","Method: ",attr(x,"indexMethod"), "\n",
              "Sample: ", attr(x, "sample"), "\n",
              "Frequency: ", attr(x, "frequency"))
  print(s)

}
