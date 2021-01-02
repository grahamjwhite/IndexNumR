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



#' Aggregates prices to unit values and quantities to sums
#'
#' A function to aggregate price and quantity data to unit values
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @return A dataframe containing columns for product identifier, time period,
#' quantities, and unit values.
#' @examples
#' # suppose the CES_sigma_2 dataset contains 12 monthly observations
#' # and suppose we want quarterly unit values.
#' df <- CES_sigma_2
#' # convert the monthly time variable into quarterly
#' df$time <- ceiling(CES_sigma_2$time/3)
#' # compute unit values using the quarterly time variable
#' unitValues(df,pvar="prices",qvar="quantities",pervar="time",prodID="prodID")
#' @export
unitValues <- function(x,pvar,qvar,pervar,prodID){

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  # number of periods
  n <- max(x[[pervar]],na.rm = TRUE)

  # loop over all periods ...
  means_it <- lapply(1:n,function(i){
    # subset period i
    xt <- x[x[[pervar]]==i,]
    # loop over all products
    means_i <- lapply(unique(xt[[prodID]]),function(id){
      # subset the period 'i', product 'id' data
      xti <- xt[xt[[prodID]]==id,]
      # calculate expenditure for this item
      exp <- sum(xti[[pvar]]*xti[[qvar]])
      # calculate total quantity
      qit <- sum(xti[[qvar]])
      # calculate unit value
      unitValue <- exp/qit
      # create a vector containing the id, time, total quantity and unit value
      result <- cbind(id,i,qit,unitValue)
      colnames(result) <- c(prodID,"period",qvar,"unitValue")
      return(result)
    })
    # bind results for products in period 'i' into a matrix
    result_i <- do.call(rbind,means_i)
  })
  # bind results for all 'i' into a matrix
  result_it <- do.call(rbind,means_it)
  return(as.data.frame(result_it, stringsAsFactors = FALSE))
}

