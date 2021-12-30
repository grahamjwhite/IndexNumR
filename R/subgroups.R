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



#' Calculate a price index on subgroups
#'
#' @param subgroup the name of the variable containing the subgroup ID. This
#' must be a factor variable, or a variable coercible to a factor.
#' @param indexFunction the function to use to calculate the index. Available
#' options are `priceIndex`, `GEKSIndex`, `GKIndex`, `WTPDIndex`.
#' @param indexArgs arguments for the price index function as a named list.
#' All arguments must be named.
#' @export
#' @examples
#' \dontrun{
#' df <- CES_sigma_2
#' df$subgroupID <- c(rep(1, 24), rep(2, 24))
#'
#' args <- list(x = df, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", indexMethod = "fisher", output = "chained")
#'
#' subgroupPriceIndexes("subgroupID", priceIndex, args)
#' }
subgroupPriceIndexes <- function(subgroup, indexFunction, indexArgs){

  within(indexArgs,{
    # sort the dataset by time period and product ID
    x <- x[order(x[[pervar]], x[[prodID]]),]
  })

  # get the groups
  groups <- sort(unique(indexArgs$x[[subgroup]]))

  # apply the price index function
  subPrices <- lapply(groups,
                      function(group){

                        # create inputs with only subgroup required
                        inputs <- indexArgs
                        inputs$x <- inputs$x[inputs$x[[subgroup]] == group,]

                        # call the index number function
                        p <- do.call(indexFunction, inputs)

                        retVal <- data.frame(prices = p,
                                             time = unique(inputs$x[[inputs$pervar]]),
                                             subgroup = group)

                        return(retVal)

                      })

  return(subPrices)

}


#' Estimate an index using year-over-year calculations
#'
#' @param freq the frequency of the data. Either "monthly" or "quarterly".
#' @inheritParams subGroupPriceIndexes
#' @export
yearOverYearIndexes <- function(freq, indexFunction, indexArgs){

  indexArgs <- within(indexArgs,{
    # sort the dataset by time period and product ID
    x <- x[order(x[[pervar]], x[[prodID]]),]

    # convert frequency to integer
    freqInt <- switch(freq,
                      "monthly" = 12,
                      "quarterly" = 4)

    # create the subgroup column using a vector of 1:freqInt
    lookup <- data.frame(min(x[[pervar]]):max(x[[pervar]]))
    lookup$subgroup <- rep(1:freqInt, len = nrow(lookup))
    colnames(lookup) <- c(pervar, "subgroup")

    x <- merge(x, lookup)

    # re-scale the time variable so that each subgroup time index starts at 1
    x[[pervar]] <- ifelse(x[[pervar]] %% freqInt == 0,
                          x[[pervar]]/freqInt,
                          (x[[pervar]] + freqInt - x[[pervar]] %% freqInt)/freqInt)

    rm(lookup, freqInt)

  })

  indexes <- subgroupPriceIndexes("subgroup", indexFunction, indexArgs)

  return(indexes)

}
