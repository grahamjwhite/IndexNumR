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


#' Compute values (price x quantity)
#'
#' Compute the total value (expenditure), for each time period in
#' the sample.
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param sample A character string specifying whether a matched sample
#' should be used.
#' @param matchPeriod A character string specifying which period is used
#' to determine the set of products used for matching. Options are
#' "following", "previous" or "fixedbase". "following" calculates the expenditures in the current period,
#' filtering out any products that do not appear in the following period.
#' "fixedbase" calculates current period expenditures, filtering out the products
#' that did not appear in the first period. "previous" is calculated similarly,
#' using the set of products in the previous period to filter the current period sample.
#' @export
values <- function(x, pvar, qvar, pervar, prodID, sample = "matched",
                   matchPeriod = "previous"){

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  # check column types
  x <- checkTypes(x, pvar, qvar, pervar)

  # check that the time period variable is continuous
  timeCheck <- isContinuous(x[[pervar]])
  if(timeCheck$result == FALSE){
    stop(paste("The time period variable is not continuous.",
               "Missing periods:", timeCheck$missing))
  }

  # sort the dataset by time period and product ID
  x <- x[order(x[[pervar]], x[[prodID]]),]

  # initialise some things
  n <- max(x[[pervar]],na.rm = TRUE)
  plist <- matrix(NA, nrow = n, ncol = 1)
  naElements <- character()

  # if matched sample requested then filter matched products
  if(sample == "matched"){

    for(i in 1:n){
      xt <- x[x[[pervar]]==i,]

      # there are two cases we can't compute
      if((i == 1 && matchPeriod == "previous") |
         (i == n && matchPeriod == "following")){
        plist[i, 1] <- NA
      }
      else {
        # set the match period
        switch(matchPeriod,
               following = {xtmatch <- x[x[[pervar]] == i + 1,]},
               previous = {xtmatch <- x[x[[pervar]] == i - 1,]},
               fixedbase = {xtmatch <- x[x[[pervar]] == 1,]})

        # filter the products using the match period
        xt <- xt[xt[[prodID]] %in% unique(xtmatch[[prodID]]),]

        # check if there are any products left
        if(nrow(xt)==0){
          plist[i,1] <- NA
          naElements <- paste0(naElements, i, sep = ",")
        }
        else {
          # calculate expenditure
          plist[i, 1] <- sum(xt[[pvar]]*xt[[qvar]])
        }
      }
    }
  }
  # if no matching required then just calculate expenditure
  else {

    for(i in 1:n){

      xt <- x[x[[pervar]]==i,]

      plist[i, 1] <- sum(xt[[pvar]]*xt[[qvar]])

    }
  }

  if(length(naElements)>0){
    warning(paste0("The following elements of the index were set to NA because there were no matched products in the two comparison periods: ", naElements))
  }

  return(plist)
}
