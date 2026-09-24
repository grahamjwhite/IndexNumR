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


#' Fill all missing prices with carry forward/backward prices
#'
#' If a missing product has a previous price then that previous price
#' is carried forward until the next real observation. If there is
#' no previous price then the next real observation is found and
#' carried backward. If a price observation is filled, and a quantity
#' variable is specified, then the corresponding quantity is set to zero.
#' Prices can be filled with no quantity variable by specifying
#' qvar = "".
#'
#' @param qvar A character string for the name of the quantity variable.
#' If there is no quantity variable you must specify qvar = "".
#' @inheritParams priceIndex
#' @return the input data frame with missing observations filled
#' @export
#' @examples
#' # create a dataset with missing prices for products 1 and 2
#' df <- CES_sigma_2[-c(1,2,14,15),]
#' imputeCarryPrices(df, "prices", "quantities", "time", "prodID")
imputeCarryPrices <- function(x, pvar, qvar, pervar, prodID){

  # list of products
  prods <- sort(unique(x[[prodID]]))
  # list of time periods
  pers <- sort(unique(x[[pervar]]))

  # position of each observation in a product x period grid
  prodPos <- match(x[[prodID]], prods)
  perPos <- match(x[[pervar]], pers)

  # grid of row numbers in x, with 0 where the product is missing in a period
  obsRow <- matrix(0L, nrow = length(prods), ncol = length(pers))
  obsRow[cbind(prodPos, perPos)] <- seq_len(nrow(x))

  missing <- obsRow == 0

  if(any(missing)){

    # for each cell, the latest available period at or before it (carry forward)
    # and the earliest available period at or after it (carry backward).
    # Cells with no available period in that direction get 0 (forward) or
    # length(pers) + 1 (backward).
    periodIndex <- col(obsRow)
    forward <- periodIndex*!missing
    backward <- ifelse(missing, length(pers) + 1L, periodIndex)
    forward <- t(apply(forward, 1, cummax))
    backward <- t(apply(backward[, ncol(backward):1, drop = FALSE], 1, cummin))[, ncol(backward):1, drop = FALSE]

    # use the previous price if there is one, otherwise the next price
    fillPeriod <- ifelse(forward > 0, forward, backward)

    toFill <- which(missing, arr.ind = TRUE)
    sourceRow <- obsRow[cbind(toFill[, 1], fillPeriod[toFill])]

    # generate the new observation rows. Indexing x with NA gives rows with the
    # same columns and column types as x, with all values missing.
    newObs <- x[rep(NA_integer_, nrow(toFill)), , drop = FALSE]
    newObs[[pervar]] <- pers[toFill[, 2]]
    newObs[[prodID]] <- prods[toFill[, 1]]
    newObs[[pvar]] <- x[[pvar]][sourceRow]
    if(qvar != ""){
      newObs[[qvar]] <- 0
    }

    # add the new observations onto the dataset
    x <- rbind(x, newObs)

    # ensure dataset still sorted by time period and product ID
    x <- x[order(x[[pervar]], x[[prodID]]),]
    rownames(x) <- NULL

  }

  return(x)

}


#' Impute quantities when only prices are available
#'
#' This procedure calculates quantities in such a way that
#' the expenditure shares on all products are equal in each
#' period. It is used to compute quantities for the predicted
#' share measure of relative price dissimilarity when there are
#' none available.
#'
#' @inheritParams priceIndex
#' @export
imputeQuantities <- function(x, pvar, pervar, prodID){

  # make sure data are sorted by time period and product ID
  x <- x[order(x[[pervar]], x[[prodID]]),]

  # total time periods
  obs <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  # generate some zero quantities to begin with
  x$quantities <- 0

  # fill the remaining missing observations with zero
  x <- fillMissing(x, pvar, "quantities", pervar, prodID,
                   priceReplace = 0, quantityReplace = 0)

  # form price matrix
  p <- matrix(x[[pvar]], nrow = obs, byrow = TRUE)

  # get the number of products available in each period
  numProds <- rowSums(replace(p, p > 0, 1))

  # calculate the imputed quantities
  imputedQty <- sapply(1:nrow(p), function(i){
    1/p[i,]*numProds[i]
  })
  imputedQty[is.infinite(imputedQty)] <- 0

  x$quantities <- as.vector(imputedQty)

  return(x)

}
