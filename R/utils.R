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

  # remove columns specified as empty strings to allow qvar = "" for
  # elementary indexes
  namesVector <- namesVector[!namesVector == ""]

  badNames <- namesVector[!(namesVector %in% goodNames)]

  if(length(badNames) >= 1){
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
    if(inherits(coerced, "try-error") | any(is.na(coerced))){
      check = FALSE
      m <- "Time period variable is not numeric and cannot be coerced to numeric"
    }
    else {
      x[[pervar]] <- coerced
    }
  }

  if(!inherits(x[[pvar]], "numeric")){
    coerced <- try(as.numeric(x[[pvar]]), silent = TRUE)
    if(inherits(coerced, "try-error") | any(is.na(coerced))){
      check = FALSE
      m <- "Price variable is not numeric and cannot be coerced to numeric"
    }
    else {
      x[[pvar]] <- coerced
    }
  }

  # don't check qvar for elementary indexes
  if(!qvar == ""){
    if(!inherits(x[[qvar]], "numeric")){
      coerced <- try(as.numeric(x[[qvar]]), silent = TRUE)
      if(inherits(coerced, "try-error") | any(is.na(coerced))){
        check = FALSE
        m <- "Quantity variable is not numeric and cannot be coerced to numeric"
      }
      else {
        x[[qvar]] <- coerced
      }
    }
  }

  if(check){
    return(x)
  }
  else {
    stop(paste("Please correct input data types.", m), call. = FALSE)
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


#' kennedyBeta
#'
#' calculate the adjusted regression coefficients for a semi-log model
#' @param x a regression object (e.g. the output from \code{lm})
#' @return a vector of adjusted coefficients
#' @keywords internal
#' @noRd
kennedyBeta <- function(x){

  coeffs <- stats::coef(x)
  vars <- diag(stats::vcov(x))

  return(coeffs - 0.5*vars)

}


#' fillMissing
#'
#' fill in missing observations
#' @param x the dataset
#' @param pvar the price variable
#' @param qvar the quantity variable
#' @param pervar the time period variable
#' @param prodID the product identifier
#' @param priceReplace what to replace missing prices with
#' @param quantityReplace what to replace missing quantities with
#' @keywords internal
#' @noRd
fillMissing <- function(x, pvar, qvar, pervar, prodID, priceReplace, quantityReplace){

  # list of time periods
  pers <- sort(unique(x[[pervar]]))
  # list of products
  prods <- sort(unique(x[[prodID]]))

  # fill out the gaps from missing/new products with replacement values.
  available <- table(x[,c(prodID, pervar)])
  if(any(available == 0)){

    # which products are not available
    toAdd <- as.data.frame(which(available == 0, arr.ind = TRUE))

    # generate the new observation row for price, quantity, time and product id
    newObs <- data.frame(rep(priceReplace, nrow(toAdd)),
                         rep(quantityReplace,nrow(toAdd)),
                         prods[toAdd[[prodID]]],
                         pers[toAdd[[pervar]]],
                         stringsAsFactors = FALSE)

    # set column names to the ones used in the input dataset
    colnames(newObs) <- c(pvar, qvar, prodID, pervar)

    # add the new observations onto the dataset
    x <- merge(x, newObs, all.x = TRUE, all.y = TRUE)

    # ensure dataset still sorted by time period and product ID
    x <- x[order(x[[pervar]], x[[prodID]]),]
  }

  return(x)

}

#' windowMatch
#'
#' match products over a window of periods
#'
#' @param x data frame of product data
#' @param pervar string for the name of the time period variable
#' @param prodID string for the name of the product identifier variable
#' @return a dataframe of matched product data
#' @keywords internal
#' @noRd
windowMatch <- function(x, pervar, prodID){

  obs <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  tab <- table(x[[pervar]], x[[prodID]])
  keep <- colnames(tab)[colSums(tab) == obs]

  return(x[x[[prodID]] %in% keep,])

}


#' Check for unique time/product combinations
#'
#' This function checks for uniqueness of time/product combinations
#' so that the priceIndex function works as expected.
#'
#' @param x a dataframe
#' @param pervar the time period variable
#' @param prodID the product identifier
#' @keywords internal
#' @noRd
#'
checkTimeProdUnique <- function(x, pervar, prodID){

  # anyDuplicated avoids building a dense time x product table,
  # which can be very large for scanner data
  check <- anyDuplicated(x[, c(pervar, prodID)]) == 0

  return(list(result = check))

}


#' checkArguments
#'
#' Checks that the arguments passed through \code{...} are valid. This catches
#' misspelled argument names (e.g. indexMmethod instead of indexMethod) that would
#' otherwise be silently absorbed by \code{...}.
#'
#' @param dots a named list of the arguments passed through \code{...}
#' @param allowed character vector of argument names that may be passed through \code{...}
#' @param funcName the name of the function whose formal arguments are used to
#' suggest corrections for misspelled argument names
#' @param allowedLabel the text used to introduce the list of allowed arguments
#' in the error message
#' @keywords internal
#' @noRd
checkArguments <- function(dots, allowed, funcName,
                           allowedLabel = "Valid additional arguments are"){

  if(length(dots) == 0){
    return(invisible(TRUE))
  }

  dotNames <- names(dots)
  if(is.null(dotNames)){
    dotNames <- rep("", length(dots))
  }

  if(any(dotNames == "")){
    stop("All additional arguments passed to ", funcName, " must be named.", call. = FALSE)
  }

  unknown <- dotNames[!dotNames %in% allowed]

  if(length(unknown) > 0){
    candidates <- c(names(formals(funcName)), allowed)
    candidates <- candidates[candidates != "..."]

    suggestions <- vapply(unknown, function(u){
      m <- agrep(u, candidates, max.distance = 2, value = TRUE, ignore.case = TRUE)
      if(length(m) > 0) paste0(" (did you mean ", m[1], "?)") else ""
    }, character(1))

    stop(paste0("Unknown argument(s) to ", funcName, ": ",
                paste0(unknown, suggestions, collapse = ", "),
                ". ", allowedLabel, ": ", paste(allowed, collapse = ", "), "."),
         call. = FALSE)
  }

  return(invisible(TRUE))

}


#' checkStartsAtOne
#'
#' checks that the time period variable starts at 1, as required
#' by the index number functions.
#' @param x vector of time periods
#' @keywords internal
#' @noRd
checkStartsAtOne <- function(x){

  if(min(x, na.rm = TRUE) != 1){
    stop("The time period variable must start at 1. The first period in the data is ",
         min(x, na.rm = TRUE), ".", call. = FALSE)
  }

  return(invisible(TRUE))

}


#' logMean
#'
#' The logarithmic mean of two vectors, with the limiting
#' value L(a, a) = a handled explicitly.
#' @param a numeric vector
#' @param b numeric vector
#' @keywords internal
#' @noRd
logMean <- function(a, b){

  # written as b*d/log(1 + d) with d = a/b - 1, which stays accurate when
  # a and b are equal or nearly equal (where (a-b)/(log(a)-log(b)) is 0/0)
  d <- a/b - 1
  result <- b*d/log1p(d)

  equal <- !is.na(d) & d == 0
  result[equal] <- b[equal]

  # the logarithmic mean is zero if either value is zero
  zero <- !is.na(a) & !is.na(b) & (a == 0 | b == 0)
  result[zero] <- 0

  return(result)

}


#' periodRows
#'
#' Finds the first and last row of each time period in a data frame
#' that is sorted by time period. Used to extract the data for a period
#' without scanning the whole data frame or copying it.
#' @param x a data frame sorted by pervar
#' @param pervar the time period variable
#' @return a list with elements start and end, indexed by period
#' @keywords internal
#' @noRd
periodRows <- function(x, pervar){

  if(any(x[[pervar]] != floor(x[[pervar]]))){
    stop("The time period variable must contain whole numbers.", call. = FALSE)
  }

  if(is.unsorted(x[[pervar]])){
    stop("Internal error: the data must be sorted by time period.", call. = FALSE)
  }

  counts <- tabulate(x[[pervar]], nbins = max(x[[pervar]]))
  ends <- cumsum(counts)
  starts <- ends - counts + 1

  return(list(start = starts, end = ends))

}


#' getPeriod
#'
#' Extracts the rows for one time period from a sorted data frame.
#' @param x a data frame sorted by time period
#' @param rows the output of periodRows
#' @param i the period to extract
#' @param cols the columns to keep
#' @keywords internal
#' @noRd
getPeriod <- function(x, rows, i, cols = names(x)){

  if(i < 1 || i > length(rows$start) || rows$end[i] < rows$start[i]){
    return(x[0, cols, drop = FALSE])
  }

  return(x[rows$start[i]:rows$end[i], cols, drop = FALSE])

}


#' prepareData
#'
#' Runs the standard input checks (column names, column types, a continuous time
#' period variable starting at 1, unique time/product combinations) and sorts the
#' data by time period and product. Sorting is required so that the price and quantity
#' vectors of two periods refer to the same products in the same order.
#' @param x a data frame
#' @param pvar the price variable
#' @param qvar the quantity variable
#' @param pervar the time period variable
#' @param prodID the product identifier
#' @return the checked and sorted data frame
#' @keywords internal
#' @noRd
prepareData <- function(x, pvar, qvar, pervar, prodID){

  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  x <- checkTypes(x, pvar, qvar, pervar)

  timeCheck <- isContinuous(x[[pervar]])
  if(timeCheck$result == FALSE){
    stop(paste("The time period variable is not continuous.",
               "Missing periods:", timeCheck$missing))
  }

  checkStartsAtOne(x[[pervar]])

  tpCheck <- checkTimeProdUnique(x, pervar, prodID)
  if(tpCheck$result == FALSE){
    stop("Products must only have one observation for each time period. If you have multiple observations on products for one or more time periods, combine this information using unitValues() or another method before calculating the price index.")
  }

  return(x[order(x[[pervar]], x[[prodID]]),])

}
