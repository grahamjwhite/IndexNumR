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

#' indLaspeyres_t
#'
#' @param p1 numeric vector of prices in period 1
#' @param p0 numeric vector of prices in period 0
#' @param q0 numeric vector of quantities in period 0
#' @keywords internal
#' @noRd
indLaspeyres_t <- function(p0, p1, q0){
  sum(q0*(p1 - p0))
}

#' indPaasche_t
#'
#' @param p1 numeric vector of prices in period 1
#' @param p0 numeric vector of prices in period 0
#' @param q1 numeric vector of quantities in period 1
#' @keywords internal
#' @noRd
indPaasche_t <- function(p0, p1, q1){
  sum(q1*(p1 - p0))
}

#' indBennet_t
#'
#' @param p1 numeric vector of prices in period 1
#' @param p0 numeric vector of prices in period 0
#' @param q0 numeric vector of quantities in period 0
#' @param q1 numeric vector of quantities in period 1
#' @keywords internal
#' @noRd
indBennet_t <- function(p0, p1, q0, q1){
  sum(0.5*(q0 + q1)*(p1 - p0))
}

#' indMontgomery_t
#'
#' @param p1 numeric vector of prices in period 1
#' @param p0 numeric vector of prices in period 0
#' @param q0 numeric vector of quantities in period 0
#' @param q1 numeric vector of quantities in period 1
#' @keywords internal
#' @noRd
indMontgomery_t <- function(p0, p1, q0, q1){
  sum(((p1*q1 - p0*q0)/(log(p1*q1)-log(p0*q0)))*log(p1/p0))
}

#' Calculate a price indicator
#'
#' This calculates a price indicator. This is calculated using the
#' differences approach to index number theory, where the change
#' in prices and quantities from one period to the next is additive.
#' Therefore, the change in total value is the sum of the change
#' in prices and the change in quantities. Such a value decomposition
#' can be obtained using \link{\code{valueDecomposition}}}.
#'
#' @param x data frame with input data
#' @param pvar character string for the name of the price column
#' @param qvar character string for the name of the quantity column
#' @param pervar character string for the name of the time period variable
#' @param prodID character string for the name of the product ID column
#' @param method character string for the indicator method. Valid options
#' are "laspeyres", "paasche", "bennet", or "montgomery".
#' @param sample whether to use a matched sample (sample = "matched")
#' @return an nx1 matrix containing the indicator
#' @export
priceIndicator <- function(x, pvar, qvar, pervar, prodID, method,
                           sample = "matched"){

  validMethods <- c("laspeyres", "paasche", "bennet", "montgomery")
  if(!(method %in% validMethods)){
    stop("Invalid method chosen")
  }

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

  # for each time period
  for(i in 2:n){

    xt0 <- x[x[[pervar]]==i-1,]
    xt1 <- x[x[[pervar]]==i,]

    # if matching requested then remove unmatched items
    if(sample=="matched"){
      xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
      xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
    }

    # set the price index element to NA if there are no
    # matches
    if(nrow(xt1)==0){
      plist[i,1] <- NA
      naElements <- paste0(naElements, i, sep = ",")
    }
    else{
      # set p and q
      p0 <- xt0[[pvar]]
      p1 <- xt1[[pvar]]
      q0 <- xt0[[qvar]]
      q1 <- xt1[[qvar]]

      # compute the index
      switch(tolower(method),
             laspeyres = {plist[i,1] <- indLaspeyres_t(p0,p1,q0)},
             paasche = {plist[i,1] <- indPaasche_t(p0,p1,q1)},
             bennet = {plist[i,1] <- indBennet_t(p0,p1,q0,q1)},
             montgomery = {plist[i,1] <- indMontgomery_t(p0,p1,q0,q1)})
    }
  }

  if(length(naElements)>0){
    warning(paste0("The following elements of the index were set to NA because there were no matched products in the two comparison periods: ", naElements))
  }

  return(plist)

}

#' Compute a volume indicator
#'
#' This computes an indicator of the change in volume,
#' using the differences approach to measuring change.
#'
#' @param x data frame with input data
#' @param pvar character string for the name of the price column
#' @param qvar character string for the name of the quantity column
#' @param pervar character string for the name of the time period variable
#' @param prodID character string for the name of the product ID column
#' @param method character string for the indicator method. Valid options
#' are "laspeyres", "paasche", "bennet", or "montgomery".
#' @param sample whether to use a matched sample (sample = "matched")
#' @return an nx1 matrix containing the indicator
#' @export
volumeIndicator <- function(x, pvar, qvar, pervar, prodID, method,
                            sample = "matched"){

  # call priceIndicator and switch prices/quantites
  priceIndicator(x, pvar = qvar, qvar = pvar, pervar, prodID, method,
                 sample)

}

#' valueDecomposition
#'
#' Perform a decomposition of value change using price
#' and volume indicators. This is an additive decomposition
#' so that change due to price plus change due to quantity
#' equals the total value change.
#'
#' @param x data frame with input data
#' @param pvar character string for the name of the price column
#' @param qvar character string for the name of the quantity column
#' @param pervar character string for the name of the time period variable
#' @param prodID character string for the name of the product ID column
#' @param priceMethod character string for the indicator method. Valid options
#' are "laspeyres", "paasche", "bennet", or "montgomery". This parameter also
#' determines the method used for the volume indicator. If a laspeyres price
#' indicator is chosen, then a paasche volume indicator is used.
#' If a paasche price indicator is used then a laspeyres volume indicator
#' is used. For bennet and montgomery indicators, the same method is
#' used for both the price and volume indicators.
#' @param sample whether to use a matched sample (sample = "matched")
#' @return a dataframe containing the price indicator, volume indicator
#' the value change and the value level.
#' @export
indicatorDecomposition <- function(x, pvar, qvar, pervar, prodID, priceMethod,
                                  sample = "matched"){

  # initialise some things
  n <- max(x[[pervar]])
  result <- matrix(NA, nrow = n, ncol = 4)


  p <- priceIndicator(x, pvar, qvar, pervar, prodID, priceMethod,
                      sample)

  switch(priceMethod,
         laspeyres = {volumeMethod <- "paasche"},
         paasche = {volumeMethod <- "laspeyres"},
         bennet = {volumeMethod <- "bennet"},
         montgomery = {volumeMethod <- "montgomery"})

  v <- volumeIndicator(x, pvar, qvar, pervar, prodID, volumeMethod,
                       sample)

  if(sample == "matched"){

    nextMatched <- values(x, pvar, qvar, pervar, prodID, sample = "matched",
                          matchPeriod = "following")
    previousMatched <- values(x, pvar, qvar, pervar, prodID, sample = "matched",
                              matchPeriod = "previous")

    for(i in 2:n){
      # price
      result[i,1] <- p[i,1]
      #volume
      result[i,2] <- v[i,1]
      # value change
      result[i,3] <- previousMatched[i,1] - nextMatched[i-1,1]
      # value level
      result[i,4] <- previousMatched[i,1]
    }

  }
  else {

    value <- values(x, pvar, qvar, pervar, prodID, sample = "unmatched")

    for(i in 2:n){
      # price
      result[i,1] <- p[i,1]
      # volume
      result[i,2] <- v[i,1]
      # value change
      result[i,3] <- value[i,1] - value[i-1,1]
      # value level
      result[i,4] <- value[i,1]
    }
  }

  result <- as.data.frame(result)
  colnames(result) <- c("price", "volume", "changes", "values")

  return(result)
}
