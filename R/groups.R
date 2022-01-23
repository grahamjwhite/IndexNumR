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



#' Calculate price indexes for product groups
#'
#' @param group the name of the variable containing the group ID. This
#' must be a factor variable, or a variable coercible to a factor.
#' @param indexFunction the function to use to calculate the index. Available
#' options are `priceIndex`, `GEKSIndex`, `GKIndex`, `WTPDIndex`.
#' @param indexArgs arguments for the price index function as a named list.
#' All arguments must be named.
#' @return a list of indexes, one for each group
#' @export
#' @examples
#' df <- CES_sigma_2
#' df$groupID <- c(rep(1, 24), rep(2, 24))
#'
#' argsList <- list(x = df, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", indexMethod = "fisher", output = "chained")
#'
#' groupIndexes("groupID", priceIndex, argsList)
#'
groupIndexes <- function(group, indexFunction, indexArgs){

  # sort the dataset by time period and product ID
  indexArgs$x <- indexArgs$x[order(indexArgs$x[[indexArgs$pervar]], indexArgs$x[[indexArgs$prodID]]),]

  # get the groups
  groups <- sort(unique(indexArgs$x[[group]]))

  # apply the price index function
  subPrices <- lapply(groups,
                      function(y){

                        # create inputs with only subgroup required
                        inputs <- indexArgs
                        inputs$x <- inputs$x[inputs$x[[group]] == y,]

                        # call the index number function
                        p <- do.call(indexFunction, inputs)

                        retVal <- data.frame(prices = p,
                                             time = unique(inputs$x[[inputs$pervar]]),
                                             group = y)
                        colnames(retVal) <- c("prices", "time", group)

                        return(retVal)

                      })

  return(subPrices)

}


#' Estimate year-over-year indexes
#'
#' Year-over-year indexes are indexes where the months or quarters of the
#' year are split in separate datasets and an index estimated on each.
#' Therefore, year-over-year indexes estimated on a dataset with five full years
#' of observations at a monthly frequency will have 12 separate indexes,
#' each with 5 observations.
#'
#' @param freq the frequency of the data. Either "monthly" or "quarterly".
#' @inheritParams groupIndexes
#' @return a list of indexes with one element for each month or quarter
#' @export
#' @examples
#' args <- list(x = CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", indexMethod = "fisher", output = "chained")
#'
#' yearOverYearIndexes("quarterly", priceIndex, args)
#'
yearOverYearIndexes <- function(freq, indexFunction, indexArgs){

  # convert frequency to integer
  freqInt <- switch(freq,
                    "monthly" = 12,
                    "quarterly" = 4)

  freqName <- switch (freq,
                      "monthly" = "month",
                      "quarterly" = "quarter"
  )

  # sort the dataset by time period and product ID
  indexArgs$x <- indexArgs$x[order(indexArgs$x[[indexArgs$pervar]], indexArgs$x[[indexArgs$prodID]]),]

  lookup <- data.frame(min(indexArgs$x[[indexArgs$pervar]]):max(indexArgs$x[[indexArgs$pervar]]))
  lookup[[freqName]] <- rep(1:freqInt, len = nrow(lookup))
  colnames(lookup) <- c(indexArgs$pervar, freqName)

  indexArgs$x <- merge(indexArgs$x, lookup)

  # re-scale the time variable so that each subgroup time index starts at 1
  indexArgs$x[[indexArgs$pervar]] <- ifelse(indexArgs$x[[indexArgs$pervar]] %% freqInt == 0,
                                  indexArgs$x[[indexArgs$pervar]]/freqInt,
                        (indexArgs$x[[indexArgs$pervar]] + freqInt - indexArgs$x[[indexArgs$pervar]] %% freqInt)/freqInt)


  indexes <- groupIndexes(freqName, indexFunction, indexArgs)

  return(indexes)

}
