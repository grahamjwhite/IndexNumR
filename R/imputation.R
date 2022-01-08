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
#'
#'
#' @inheritParams priceIndex
#' @keywords internal
#' @noRd
imputeCarryPrices <- function(x, pvar, qvar, pervar, prodID){

  # list of products
  prods <- sort(unique(x[[prodID]]))
  # list of time periods
  pers <- sort(unique(x[[pervar]]))
  # number of periods
  n <- length(pers)

  # cross-tabulate products and time periods
  available <- table(x[,c(prodID, pervar)])

  if(any(available == 0)){

    # which products need filling
    prods <- prods[rowSums(available) < n]

    # get the fill data for each product
    fillData <- lapply(prods, function(prod){

      # time indices we need to fill
      toFill <- as.numeric(colnames(available)[available[rownames(available) == prod,] == 0])

      xprod <- x[x[[prodID]] == prod,]

      fillPrices <- lapply(toFill, function(period){

        # find previous periods that not missing for the given product
        previousPeriods <- xprod[[pervar]][xprod[[pervar]] < period & !(xprod[[pervar]] %in% toFill)]

        # if we found prior non-missing periods, use the latest, otherwise look for a carry backward price
        if(length(previousPeriods) > 0){
          carryForwardPeriod <- max(previousPeriods, na.rm = TRUE)
          fillPrice <- xprod[[pvar]][xprod[[pervar]] == carryForwardPeriod]
        } else if(length(previousPeriods) == 0){
          carryBackwardPeriod <- min(xprod[[pervar]][xprod[[pervar]] > period & !(xprod[[pervar]] %in% toFill)], na.rm = TRUE)
          fillPrice <- xprod[[pvar]][xprod[[pervar]] == carryBackwardPeriod]
        }

        if(qvar == ""){
          retVal <- c(period, prod, fillPrice)
          names(retVal) <- c(pervar, prodID, pvar)
        } else {
          retVal <- c(period, prod, fillPrice, 0)
          names(retVal) <- c(pervar, prodID, pvar, qvar)
        }

        return(retVal)

      })

      fillPrices <- do.call(rbind, fillPrices)
      return(fillPrices)

    })

    # generate the new observation rows for price, quantity, time and product id
    newObs <- do.call(rbind, fillData)

    # add the new observations onto the dataset
    x <- merge(x, newObs, all.x = TRUE, all.y = TRUE)

    # convert columns back to numeric
    x[[pvar]] <- as.numeric(x[[pvar]])
    if(!qvar == "") x[[qvar]] <- as.numeric(x[[qvar]])
    x[[pervar]] <- as.numeric(x[[pervar]])

    # ensure dataset still sorted by time period and product ID
    x <- x[order(x[[pervar]], x[[prodID]]),]

  }

  return(x)

}


#' Impute quantities when only prices are available
#'
#' @inheritParams priceIndex
#' @keywords internal
#' @noRd
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
