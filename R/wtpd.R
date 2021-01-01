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


#' this computes the weighted time product dummy method for a window
#' of data. This is not exposed to the user and is called by WTPDIndex()
#' @keywords internal
#' @noRd
wtpd_w <- function(x, pvar, qvar, pervar, prodID, sample){

  # total time periods
  obs <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  # if matching requested, keep only products that occur through the whole window
  if(sample == "matched"){
    x <- windowMatch(x, pervar, prodID)
  }
  else {
    # fill out the gaps from missing/new products with zeros.
    # this makes sure that the dimensions of all vectors/matrices
    # are conformable for the calculations
    x <- fillMissing(x, pvar, qvar, pervar, prodID, priceReplace = 0, quantityReplace = 0)
  }

  # list of time periods
  pers <- sort(unique(x[[pervar]]))
  # list of products
  prods <- sort(unique(x[[prodID]]))
  # total number of products
  n <- length(prods)

  # share of expenditure on each product in each time period
  stn <- matrix(0, nrow = obs, ncol = n)
  # expenditures matrix
  etn <- matrix(0, nrow = obs, ncol = n)
  # wnj
  wnj <- matrix(0, nrow = n, ncol = n)

  pmat <- matrix(x[[pvar]], nrow = obs, byrow = TRUE)
  qmat <- matrix(x[[qvar]], nrow = obs, byrow = TRUE)
  etn <- Reduce(`*`, list(pmat, qmat))

  # replace NAs with zeros
  etn <- replace(etn, is.na(etn), 0)

  # calculate expenditure shares
  et <- rowSums(etn, na.rm = TRUE)
  for(i in 1:obs){
    stn[i,] <- etn[i,]/et[i]
  }

  # calculate wtnj
  wtnj <- lapply(1:obs, function(xx){
    result <- stn[xx,]%*%t(stn[xx,])
    diag(result) <- 0
    return(result)
  })

  # calculate wnj (summing wtnj over time)
  wnj <- Reduce(`+`, lapply(wtnj, function(x){replace(x, is.na(x), 0)}))

  # calculate ftnj
  sums <- rowSums(wnj, na.rm = TRUE)
  ftnj <- lapply(wtnj, function(xx){
    result <- matrix(NA, nrow = n, ncol = n)
    for(i in 1:n){
      result[,i] <- xx[,i]/sums[i]
    }
    return(t(result))
  })

  # calculate fnj
  fnj <- t(apply(wnj, 1, function(xx){xx/sum(xx, na.rm = TRUE)}))

  # calculate fn
  ydiff <- lapply(1:obs, function(xx){
    temp <- x[x[[pervar]] == pers[xx],]
    result <- matrix(NA, nrow = n, ncol = n)
    for(m in 1:n){
      ym <- temp[temp[[prodID]] == prods[m],]
      for(j in 1:n){
        yj <- temp[temp[[prodID]] == prods[j],]
        if(!(nrow(ym) == 0 | nrow(yj) == 0)){
          result[m,j] <- (log(ym[[pvar]]) - log(yj[[pvar]]))*ftnj[[xx]][m,j]
        }
      }
    }
    return(result)
  })

  fn <- Reduce(`+`, lapply(ydiff, function(x){replace(x, is.na(x), 0)}))
  fn <- rowSums(fn)

  # Compute I - F
  I <- diag(1, nrow = n, ncol = n)
  I_F <- I - fnj

  # Now solve for b and append bN = 0
  b <- c(solve(I_F[-n, -n], fn[-n]), 0)
  b <- exp(b)

  # With b we can calculate P
  pindex <- matrix(0, nrow = obs, ncol = 1)
  for(i in 1:obs){

    p <- x[[pvar]][x[[pervar]] == pers[i]]
    s <- stn[i,]

    pindex[i,1] <- prod((p/b)^s, na.rm = TRUE)

  }

  # normalise to first period
  pindex <- pindex/pindex[1,1]

  return(pindex)

}

#' Compute a weighted time-product-dummy multilateral index
#'
#' A function to calculate a weighted-time-product-dummy multilateral index.
#'
#' When there are missing values in the dataset (e.g., from new or disappearing
#' products), the default option is to treat the missing prices and quantities
#' as zero. An alternative is to use a matched sample, where only products that
#' appear throughout each window in the calculation are kept.
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param sample set to "matched" to only use products that occur
#' across all periods in a given window. Default is not to match.
#' @param window An integer specifying the length of the window.
#' @param splice A character string specifying the splicing method. Valid methods are
#' window, movement, half, mean, fbew or fbmw. The default is mean.
#' @details The splicing methods are used to update the price index when new data become
#' available without changing prior index values. The window, movement, half and mean splices
#' use the most recent index value as the base period, which is multiplied by a price movement
#' computed using new data. The fbew (Fixed Base Expanding Window) and fbmw (Fixed Base Moving
#' Window) use a fixed base onto which the price movement using new data is applied. The base
#' period is updated periodically. IndexNumR calculates which periods are the base periods using
#' \code{seq(from = 1, to = n, by = window - 1)}, so the data must be set up correctly and the
#' right window length chosen. For example, if you have monthly data and want December
#' of each year to be the base period, then the first period in the data must be December
#' and the window must be set to 13.
#' @examples
#' # compute a wtpd index with mean splicing
#' WTPDIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", window=11, splice = "mean")
#' @references Ivancic, L., W.E. Diewert and K.J. Fox (2011), "Scanner Data,
#' Time Aggregation and the Construction of Price Indexes", Journal of
#' Econometrics 161, 24-35.
#' @export
WTPDIndex <- function(x, pvar, qvar, pervar, prodID, sample = "", window = 13, splice = "mean"){

  # check that only valid splice methods are chosen
  if(!(tolower(splice) %in% c("mean", "window", "movement", "half", "fbew", "fbmw"))){
    stop("Not a valid splicing method.")
  }

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  # check that the time period variable is continuous
  timeCheck <- isContinuous(x[[pervar]])
  if(timeCheck$result == FALSE){
    stop(paste("The time period variable is not continuous.",
               "Missing periods:", timeCheck$missing))
  }

  # check that columns are the right class
  x <- checkTypes(x, pvar, qvar, pervar)

  # get the number of periods
  n <- max(x[[pervar]], na.rm = TRUE)
  if(n < window){
    stop("The window length exceeds the number of periods in the data")
  }

  # sort the dataset by time period and product ID
  x <- x[order(x[[pervar]], x[[prodID]]),]

  # initialise some matrices
  # final price index
  pWTPD <- matrix(0, nrow = n, ncol = 1)

  # set the sequence of base periods for fbew and fbmw splices
  bases <- seq(from = 1, to = n, by = window - 1)

  # first estimate a WTPD index for the first (window) observations
  # subset the window of data to use
  xWindow <- x[x[[pervar]] >= 1 & x[[pervar]] <= window,]

  # call wtpd_w on first window
  pWTPD[1:window, 1] <- wtpd_w(xWindow, pvar, qvar, pervar, prodID, sample)

  # use a splicing method to compute the rest of the index
  if(n > window){
    for(i in 2:(n-window+1)){

      # find the base period for fbew and fbmw splices
      base <- max(bases[bases <= i + window - 2])

      # set the old window
      if(i==2){
        old <- pWTPD[(i-1):(i+window-2), 1]
      }
      else {
        old <- new
      }

      # set the base value for fbew
      fbewBase <- pWTPD[base,1]

      # fetch the next window of data
      xWindow <- x[x[[pervar]]>=i & x[[pervar]] < i + window,]

      # call wtpd_w on this window
      new <- wtpd_w(xWindow, pvar, qvar, pervar, prodID, sample)

      # splice the new datapoint on
      switch(splice,
             fbew = {pWTPD[i+window-1,1] <- fbewBase*new[length(new)]},
             fbmw = {pWTPD[i+window-1,1] <- fbewBase*new[length(new)]/new[length(new)-(i+window-1-base)]},
             pWTPD[i+window-1,1] <- splice_t(pWTPD[i+window-2,1], old, new, method=splice))

    }
  }

  return(pWTPD)

}
