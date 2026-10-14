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

#' toNumeric
#'
#' Converts a column to numeric. Factors are converted through their labels,
#' because as.numeric on a factor returns the level codes rather than the values.
#' @param v the column to convert
#' @param label a description of the column for the error message
#' @return the converted column
#' @keywords internal
#' @noRd
toNumeric <- function(v, label){

  if(is.numeric(v)){
    return(v)
  }

  if(is.factor(v)){
    v <- as.character(v)
  }

  coerced <- tryCatch(suppressWarnings(as.numeric(v)), error = function(e) NULL)

  # values that were present but could not be converted. Values that are
  # already missing are left for the other checks to deal with.
  failed <- if(is.null(coerced)) rep(TRUE, length(v)) else is.na(coerced) & !is.na(v)

  if(any(failed)){
    bad <- utils::head(unique(v[failed]), 3)
    stop("Please correct input data types. ", label,
         " is not numeric and cannot be coerced to numeric (e.g. ",
         paste0("'", bad, "'", collapse = ", "), ")", call. = FALSE)
  }

  return(coerced)

}

#' checkTypes
#'
#' Checks that the columns of the input data frame have the right types, and
#' converts them where possible. Prices, quantities and time periods must be
#' numeric, and time periods must be whole numbers. Unused levels of a factor
#' product identifier are dropped, so that they are not treated as products.
#' Variables given as an empty string are not checked.
#' @param x the dataframe to check
#' @param pvar name of price variable
#' @param qvar name of quantity variable
#' @param pervar name of time period variable
#' @param prodID name of the product identifier
#' @return if all checks pass, the input dataframe with the columns
#' coerced to the correct types, otherwise an error.
#' @keywords internal
#' @noRd
checkTypes <- function(x, pvar, qvar, pervar, prodID = ""){

  if(!pervar == ""){
    if(inherits(x[[pervar]], c("Date", "POSIXt"))){
      stop("Please correct input data types. The time period variable is a date. ",
           "Convert it to a time period index with monthIndex(), weekIndex(), ",
           "quarterIndex() or yearIndex() first.", call. = FALSE)
    }
    x[[pervar]] <- toNumeric(x[[pervar]], "Time period variable")
    periods <- x[[pervar]][!is.na(x[[pervar]])]
    if(any(periods != round(periods))){
      stop("Please correct input data types. The time period variable must ",
           "contain whole numbers.", call. = FALSE)
    }
  }

  if(!pvar == ""){
    x[[pvar]] <- toNumeric(x[[pvar]], "Price variable")
  }

  # qvar is empty for elementary indexes
  if(!qvar == ""){
    x[[qvar]] <- toNumeric(x[[qvar]], "Quantity variable")
  }

  if(!prodID == "" && is.factor(x[[prodID]])){
    x[[prodID]] <- droplevels(x[[prodID]])
  }

  return(x)

}

#' validateInput
#'
#' The standard checks run on a dataset passed to an exported function. Converts
#' data.tables and tibbles to a plain data frame, because the package relies on
#' base data frame indexing, checks that the named columns exist and checks and
#' converts their types. Variables given as an empty string are not checked.
#' @param x the input data
#' @param pvar the price variable
#' @param qvar the quantity variable
#' @param pervar the time period variable
#' @param prodID the product identifier
#' @return the checked data frame
#' @keywords internal
#' @noRd
validateInput <- function(x, pvar, qvar, pervar, prodID){

  if(!is.data.frame(x)){
    stop("The input data must be a data frame.", call. = FALSE)
  }

  # as.data.frame copies a data.table, so the caller's object is not modified
  if(!identical(class(x), "data.frame")){
    x <- as.data.frame(x)
  }

  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  return(checkTypes(x, pvar, qvar, pervar, prodID))

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
    toAdd <- which(available == 0, arr.ind = TRUE)

    # generate the new observation rows. Indexing x with NA gives rows with the
    # same columns and column types as x, with all values missing.
    newObs <- x[rep(NA_integer_, nrow(toAdd)), , drop = FALSE]
    newObs[[pvar]] <- priceReplace
    newObs[[qvar]] <- quantityReplace
    # map the table positions back to product and period values (via the
    # table names, since a factor may have levels that are not in the data)
    newObs[[prodID]] <- prods[match(dimnames(available)[[1]][toAdd[, 1]], as.character(prods))]
    newObs[[pervar]] <- pers[match(dimnames(available)[[2]][toAdd[, 2]], as.character(pers))]

    # add the new observations onto the dataset
    x <- rbind(x, newObs)

    # ensure dataset still sorted by time period and product ID
    x <- x[order(x[[pervar]], x[[prodID]]),]
    rownames(x) <- NULL
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

  # products have at most one observation per period, so a product is in
  # every period of the window if it has obs observations. Counting the
  # observations for each product avoids a dense time x product table.
  counts <- table(x[[prodID]])
  keep <- names(counts)[counts == obs]

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

  x <- validateInput(x, pvar, qvar, pervar, prodID)

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
