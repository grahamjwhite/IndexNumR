# IndexNumR: a package for index number computation
# Copyright (C) 2018 Graham J. White (g.white@unswalumni.com)
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.



#' geomean
#'
#' function for the geometric mean of a vector
#' @param x a numeric vector
#' @keywords internal
#' @noRd
geomean <- function(x, na.rm = TRUE){
  if(na.rm == TRUE){
    x <- x[!is.na(x)]
  }
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
#' @noRd
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

#' checkTypes
#'
#' checks that the columns of the input matrix are right
#' @param x the dataframe to check
#' @param pvar name of price variable
#' @param qvar name of quantity variable
#' @param pervar name of time period variable
#' @return if all checks pass, the original dataframe. If some
#' columns are the wrong type then either an error, or the
#' input dataframe coerced to the correct types.
#' @keywords internal
#' @noRd
checkTypes <- function(x, pvar, qvar, pervar){

  check <- TRUE

  if(!inherits(x[[pervar]], "numeric")){
    coerced <- try(as.numeric(x[[pervar]]), silent = TRUE)
    if(inherits(coerced, "try-error")){
      check = FALSE
      message("Time period variable is not numeric and cannot be coerced to numeric")
    }
    else {
      x[[pervar]] <- coerced
    }
  }

  if(!inherits(x[[pvar]], "numeric")){
    coerced <- try(as.numeric(x[[pvar]]), silent = TRUE)
    if(inherits(coerced, "try-error")){
      check = FALSE
      message("Price variable is not numeric and cannot be coerced to numeric")
    }
    else {
      x[[pvar]] <- coerced
    }
  }

  if(!inherits(x[[qvar]], "numeric")){
    coerced <- try(as.numeric(x[[qvar]]), silent = TRUE)
    if(inherits(coerced, "try-error")){
      check = FALSE
      message("Quantity variable is not numeric and cannot be coerced to numeric")
    }
    else {
      x[[qvar]] <- coerced
    }
  }

  if(check){
    return(x)
  }
  else {
    stop("Please correct input data types")
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
#' @noRd
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

#' daysInMonth
#'
#' calculate the number of days in the month that the given date falls
#' @param x A date
#' @return The number of days in the month in which x falls
#' @keywords internal
#' @noRd
daysInMonth <- function(x){
  month <- format(x,"%m")
  switch (month,
    "01" = return(31),
    "02" = {
      year <- format(x,"%Y")
      return(as.numeric(as.Date(paste0("01-03-",year),format="%d-%m-%Y") -
                     as.Date(paste0("01-02-",year),format="%d-%m-%Y")))
    },
    "03" = return(31),
    "04" = return(30),
    "05" = return(31),
    "06" = return(30),
    "07" = return(31),
    "08" = return(31),
    "09" = return(30),
    "10" = return(31),
    "11" = return(30),
    "12" = return(31)
  )

}


