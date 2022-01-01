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



#' Compute expenditure shares for each product and time period
#'
#' @inheritParams priceIndex
#' @return an n by p data frame of expenditure shares
#' @export
shares <- function(x, pvar, qvar, pervar, prodID){

  # fill missing observations with zero, so we can use matrix algebra
  x <- fillMissing(x, pvar, qvar, pervar, prodID, priceReplace = 0, quantityReplace = 0)

  # sort by time period and product ID
  x <- x[order(x[[pervar]], x[[prodID]]),]

  # define some things
  # list of products
  prods <- sort(unique(x[[prodID]]))
  # total number of products
  n <- length(prods)
  # total time periods
  obs <- max(x[[pervar]]) - min(x[[pervar]]) + 1
  # share of expenditure on each product in each time period
  stn <- matrix(0, nrow = obs, ncol = n)

  pmat <- matrix(x[[pvar]], nrow = obs, byrow = TRUE)
  qtn <- matrix(x[[qvar]], nrow = obs, byrow = TRUE)
  etn <- Reduce(`*`, list(pmat, qtn))

  # calculate expenditure shares
  et <- rowSums(etn)
  for(i in 1:obs){
    stn[i,] <- etn[i,]/et[i]
  }

  retVal <- data.frame(stn,
                       row.names = unique(x[[pervar]]))
  colnames(retVal) <- unique(x[[prodID]])

  return(retVal)

}


#' Predicted shares for predicted share relative price dissimilarity
#'
#' @inheritParams priceIndex
#' @return a list of matrices
#' @export
predictedShares <- function(x, pvar, qvar, pervar, prodID){

  # fill any missing observations with zero
  x <- fillMissing(x, pvar, qvar, pervar, prodID, priceReplace = 0, quantityReplace = 0)

  # list of products
  prods <- sort(unique(x[[prodID]]))
  # list of time periods
  pers <- sort(unique(x[[pervar]]))

  # expenditure for each product, time period and price vector
  eztn <- lapply(prods, function(prod){

    xprod <- x[x[[prodID]] == prod,]

    xprod[[qvar]]%*%t(xprod[[pvar]])

  })

  # total expenditure for each time period and price vector
  ezt <- Reduce(`+`, eztn)

  # shares for each product, time period and price vector
  sztn <- lapply(eztn, `/`, ezt)

  return(sztn)

}
