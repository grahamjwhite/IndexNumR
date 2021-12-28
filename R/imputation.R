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

        retVal <- c(period, prod, fillPrice, 0)
        names(retVal) <- c(pervar, prodID, pvar, qvar)

        return(retVal)

      })

      fillPrices <- do.call(rbind, fillPrices)
      return(fillPrices)

    })

    # generate the new observation rows for price, quantity, time and product id
    newObs <- do.call(rbind, fillData)

    # add the new observations onto the dataset
    x <- merge(x, newObs, all.x = TRUE, all.y = TRUE)

    # ensure dataset still sorted by time period and product ID
    x <- x[order(x[[pervar]], x[[prodID]]),]
  }

  return(x)

}
