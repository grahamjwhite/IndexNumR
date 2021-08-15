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


#' calculate a two-step index
#'
#' @inheritParams priceIndex
#' @param subgroup the name of the variable containing the subgroup ID. This
#' must be a factor variable, or a variable coercible to a factor.
#' @param stepOneFunction the index number function to be used for the the first
#' step aggregation.
#' @param stepTwoFunction the index number function to be used for the second
#' step aggregation.
#' @param stepOneArgs arguments to be passed to the price index function
#' in the first step as a named list. All arguments must be named.
#' @param stepTwoArgs arguments to be passed to the price index function
#' in the second step as a named list. All arguments must be named.
#' @export
#'
twoStepIndex <- function(x, pvar, qvar, pervar, prodID, subgroup,
                          output = "pop",
                          stepOneFunction = priceIndex,
                          stepTwoFunction = priceIndex,
                          stepOneArgs = list(),
                          stepTwoArgs = list()){

  x <- x[order(x[[pervar]], x[[prodID]]),]

  stepOneArgs$pvar <- pvar
  stepOneArgs$qvar <- qvar
  stepOneArgs$pervar <- pervar
  stepOneArgs$prodID <- prodID
  stepOneArgs$output <- output

  # append defaults if not supplied
  stepOneFormals <- formals(stepOneFunction)
  stepTwoFormals <- formals(stepTwoFunction)

  setFormals <- function(list, formals){
    for(formal in names(formals)){
      if(is.null(list[[formal]])){
        list[[formal]] <- formals[[formal]]
      }
    }
    return(list)
  }

  stepOneArgs <- setFormals(stepOneArgs, stepOneFormals)
  stepTwoArgs <- setFormals(stepTwoArgs, stepTwoFormals)

  # calculate the index by comparing two periods at a time
  n <- max(x[[pervar]])
  secondIndex <- matrix(NA, nrow = n, ncol = 1)
  secondIndex[1, 1] <- 1
  multiIndex <- ifelse(deparse(substitute(stepOneFunction)) %in% c("GEKSIndex", "GKIndex", "WTPDIndex"),
                       TRUE, FALSE)

  for(i in 2:n){

    # subset the two periods
    if(stepOneArgs$output %in% c("pop", "chained")){
      basePeriod <- i-1
    } else {
      basePeriod <- 1
    }
    xt <- x[x[[pervar]] %in% c(basePeriod, i),]

    # time period variable must start at 1 for subgroup indexes
    xt[[pervar]][xt[[pervar]] == basePeriod] <- 1
    xt[[pervar]][xt[[pervar]] == i] <- 2

    # set the two period dataset for the first step
    stepOneArgs$x <- xt

    # calculate the price index for each group
    groupIndexes <- subgroupPriceIndexes(subgroup = subgroup,
                                         indexFunction = stepOneFunction,
                                         indexArgs = stepOneArgs)

    # calculate the quantities, using the right matching
    matchPeriod <- if (multiIndex) {
      "previous"
    } else {
      switch (
        tolower(stepOneArgs$indexMethod),
        "laspeyres" = "following",
        "paasche" = "previous",
        "previous"
      )
    }

    xtsplit <- split(xt, xt[[subgroup]])

    vt <- lapply(xtsplit, values, pvar = pvar, qvar = qvar, pervar = pervar, prodID = prodID,
                 sample = "matched", matchPeriod = matchPeriod)
    pt <- lapply(groupIndexes, `[[`, "prices")
    pt <- lapply(pt, as.matrix)
    qt <- mapply(function(x, y){x/y}, vt, pt, SIMPLIFY = FALSE)

    # column bind prices and quantities
    xt2 <- mapply(function(x, y){cbind(x, y)}, groupIndexes, qt, SIMPLIFY = FALSE)

    # combine the prices and implied quantities into a new dataset
    newData <- do.call(rbind, xt2)

    # set args for second step
    stepTwoArgs$x <- newData
    stepTwoArgs$pvar <- "prices"
    stepTwoArgs$qvar <- "y"
    stepTwoArgs$pervar <- "time"
    stepTwoArgs$prodID <- "subgroup"

    # estimate the second-step index.
    secondIndex[i,1] <- do.call(stepTwoFunction, args = stepTwoArgs)[2,1]

  }

  # cumulate for a chained index, return as vector for fixed base index
  if(tolower(output == "chained")){
    result <- apply(secondIndex, 2, cumprod)
  } else {
    result <- secondIndex
  }

  return(result)

}

