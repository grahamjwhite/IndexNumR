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



#' GEKS_w
#'
#' Function to compute a GEKS index over a window.
#' The window size is assumed to be the number of periods available in pervar.
#' This is not exposed to the user because GEKSIndex calls this
#' @keywords internal
#' @noRd
GEKS_w <- function(x, pvar, qvar, pervar, indexMethod="tornqvist", prodID,
                   sample="matched", biasAdjust, weights){

  # get the window length
  window <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  # initialise some matrices
  # matrix of all price indices using each month as the base period
  pindices <- matrix(0, nrow = window, ncol = window)

  # get the vector of period indices that are inside the window
  pi <- unique(x[[pervar]])

  # initialise a vector for storing NA pair information
  naPairs <- character()

  # for every period in the window...
  for(j in 1:window){

    # for every period in the window...
    for(k in 1:window){
      # if j=k then the index is 1
      if(j==k){
        pindices[j,k] <- 1
      }
      # if we're below the diagonal, then use symmetry to
      # save computation time
      else if(j>k){
        pindices[j,k] <- 1/pindices[k,j]
      }
      else {
        # set the period pi(j) = base period
        xt0 <- x[x[[pervar]] == pi[j],]
        # set the period pi(k) = period '1'
        xt1 <- x[x[[pervar]] == pi[k],]

        # if user asked for matching, get matched samples
        if(sample=="matched"){
          xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
          xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
        }

        # set the price index element to NA if there are no
        # matches
        if(nrow(xt1)==0){
          pindices[j,k] <- NA
          naPairs <- paste0(naPairs, paste0("(",j,",",k,")"), collapse = ",")
        }
        else{
          # set the price and quantity vectors
          p0 <- xt0[[pvar]]
          p1 <- xt1[[pvar]]
          q0 <- xt0[[qvar]]
          q1 <- xt1[[qvar]]

          # calculate the price index for 'base' period j and 'next' period k
          switch(tolower(indexMethod),
                 fisher = {pindices[j,k] <- fisher_t(p0, p1, q0, q1)},
                 tornqvist = {pindices[j,k] <- tornqvist_t(p0, p1, q0, q1)},
                 tpd = {pindices[j,k] <- tpd_t(p0, p1, q0, q1, xt0[[prodID]], xt1[[prodID]], biasAdjust, weights)},
                 walsh = {pindices[j,k] <- walsh_t(p0, p1, q0, q1)},
                 jevons = {pindices[j,k] <- jevons_t(p0, p1)})
        }

      }
    }
  }
  # compute the geometric mean of each column of the price indices matrix
  pgeo <- apply(pindices, 2, geomean, na.rm = TRUE)

  # normalise to the first period
  pgeo <- pgeo/pgeo[1]

  return(list(pgeo = pgeo, naPairs = naPairs))
}

#' Compute a GEKS multilateral index
#'
#' A function to calculate a GEKS multilateral price index
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param indexMethod A character string to select the index number method. Valid index
#' number methods are fisher, tornqvist, tpd, jevons or walsh. The default is tornqvist.
#' @param sample A character string specifying whether matching is to be performed.
#' The default is to use matching.
#' If sample=matched then any products that are not present in comparison periods
#' are removed prior to estimating the index for those periods.
#' @param window An integer specifying the length of the window.
#' @param splice A character string specifying the splicing method. Valid methods are
#' window, movement, half, mean, fbew or fbmw, wisp, hasp or mean_pub. The default is mean.
#' See details for important considerations when using fbew and fbmw.
#' @param biasAdjust whether to adjust for bias in the coefficients of the bilateral TPD index.
#' The default is FALSE because making this adjustment will break transitivity of the
#' GEKS index.
#' @param weights the type of weighting for the bilateral TPD index. Options are
#' "unweighted" to use ordinary least squares, "shares" to use weighted least squares
#' with expenditure share weights, and "average" to use weighted least squares
#' with the average of the expenditure shares over the two periods. See details for more
#' information
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
#' # compute a GEKS mutlilateral index with mean splicing
#' GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", indexMethod = "tornqvist", window=11, splice = "mean")
#'
#' # compute a GEKS multilateral index with window splicing and the Fisher index method
#' GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", indexMethod = "fisher", window=11, splice = "mean")
#'
#' @references Ivancic, L., W.E. Diewert and K.J. Fox (2011), "Scanner Data,
#' Time Aggregation and the Construction of Price Indexes", Journal of
#' Econometrics 161, 24-35.
#' @export
GEKSIndex <- function(x, pvar, qvar, pervar,indexMethod = "tornqvist", prodID,
                      sample = "matched", window = 13, splice = "mean", biasAdjust = FALSE,
                      weights = "average"){

  # check that only valid index methods are chosen
  if(!(tolower(indexMethod) %in% c("fisher","tornqvist", "tpd", "walsh", "jevons"))){
    stop("Not a valid index number method.")
  }

  # check that only valid splice methods are chosen
  if(!(tolower(splice) %in% c("mean", "window", "movement", "half", "fbew", "fbmw", "wisp", "hasp", "mean_pub"))){
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
  n <- max(x[[pervar]],na.rm = TRUE)
  if(n<window){
    stop("The window length exceeds the number of periods in the data")
  }

  # sort the dataset by time period and product ID
  x <- x[order(x[[pervar]], x[[prodID]]),]

  # initialise some matrices
  # final price index
  pGEKS <- matrix(0, nrow = n, ncol = 1)

  # set the sequence of base periods for fbew and fbmw splices
  bases <- seq(from = 1, to = n, by = window - 1)

  # first estimate a GEKS index for the first (window) observations
  # subset the window of data to use
  xWindow <- x[x[[pervar]] >= 1 & x[[pervar]] <= window,]

  # call GEKS_w on first window
  tempGEK <- GEKS_w(xWindow, pvar, qvar, pervar, indexMethod, prodID,
                    sample, biasAdjust, weights)
  pGEKS[1:window,1] <- tempGEK$pgeo

  # initiate a vector of warnings for NAs
  if(length(tempGEK$naPairs) > 0){
    naWarn <- paste0("1 to ",window,": ",tempGEK$naPairs,"\n")
  }
  else{
    naWarn <- character()
  }

  # use a splicing method to compute the rest of the index
  if(n>window){
    for(i in 2:(n-window+1)){

      # find the base period for fbew and fbmw splices
      base <- max(bases[bases <= i + window - 2])

      # set the old GEKS window
      if(i==2){
        old <- pGEKS[(i-1):(i+window-2),1]
      }
      else {
        old <- new
      }

      # set the base value for fbew
      fbewBase <- pGEKS[base,1]

      # fetch the next window of data
      if(splice == "fbew"){
        xWindow <- x[x[[pervar]] >= base & x[[pervar]] < i + window,]
      }
      else {
        xWindow <- x[x[[pervar]] >= i & x[[pervar]] < i + window,]
      }

      # call GEKS_w on this window
      tempGEK <- GEKS_w(xWindow, pvar, qvar, pervar, indexMethod, prodID, sample, biasAdjust, weights)
      new <- tempGEK$pgeo

      if(length(tempGEK$naPairs) > 0){
        naWarn <- paste0(naWarn, i, " to ",i+window-1,": ",
                         tempGEK$naPairs, "\n")
      }

      # splice the new datapoint on
      switch(splice,
             fbew = {pGEKS[i+window-1,1] <- fbewBase*new[length(new)]},
             fbmw = {pGEKS[i+window-1,1] <- fbewBase*new[length(new)]/new[length(new)-(i+window-1-base)]},
             wisp = {pGEKS[i+window-1,1] <- splice_t(pGEKS[i+window-2,1], pGEKS[(i-1):(i+window-2)], new, method="window")},
             hasp = {pGEKS[i+window-1,1] <- splice_t(pGEKS[i+window-2,1], pGEKS[(i-1):(i+window-2)], new, method="half")},
             mean_pub = {pGEKS[i+window-1,1] <- splice_t(pGEKS[i+window-2,1], pGEKS[(i-1):(i+window-2)], new, method="mean")},
             pGEKS[i+window-1,1] <- splice_t(pGEKS[i+window-2,1], old, new, method=splice))

    }
  }

  # if there were periods where there were no overlapping products then
  # print a warning
  if(length(naWarn) > 0){
    warning(paste0("The following windows contained bilateral comparisons where no overlapping products were found: \n",
                   "Window: Pairs \n",naWarn))
  }

  return(pGEKS)
}



