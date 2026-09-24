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
#' This is not exposed to the user because GEKSIndex calls this
#' @param periods the time periods in the window
#' @param bilateral a function of two periods (j, k) that returns the bilateral
#' index between base period j and comparison period k, or NA if there are
#' no matched products
#' @keywords internal
#' @noRd
GEKS_w <- function(periods, bilateral){

  # get the window length
  window <- length(periods)

  # initialise some matrices
  # matrix of all price indices using each month as the base period
  pindices <- matrix(0, nrow = window, ncol = window)

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
        # calculate the price index for 'base' period j and 'next' period k
        pindices[j,k] <- bilateral(periods[j], periods[k])

        if(is.na(pindices[j,k])){
          naPairs <- c(naPairs, paste0("(",j,",",k,")"))
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


#' geksBilateral
#'
#' Calculates the bilateral index between two periods for the GEKS index.
#' @param x the data, sorted by time period and product
#' @param rows the output of periodRows(x, pervar)
#' @param cols the columns needed for the calculation
#' @param j the base period
#' @param k the comparison period
#' @return the bilateral index, or NA if there are no matched products
#' @keywords internal
#' @noRd
geksBilateral <- function(x, rows, cols, j, k, pvar, qvar, prodID, indexMethod,
                          sample, biasAdjust, weights){

  # set the period j = base period
  xt0 <- getPeriod(x, rows, j, cols)
  # set the period k = period '1'
  xt1 <- getPeriod(x, rows, k, cols)

  # if user asked for matching, get matched samples
  if(sample=="matched"){
    xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
    xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
  }
  # without matching, only the tpd method can handle different product sets
  else if(indexMethod != "tpd" && !setequal(xt0[[prodID]], xt1[[prodID]])){
    stop("The products differ between periods ", j, " and ", k,
         ", which the ", indexMethod, " index method cannot handle without matching. ",
         "Use sample = \"matched\", impute the missing prices with imputePrices = \"carry\", ",
         "or use the tpd index method.")
  }

  # the price index is NA if there are no matches
  if(nrow(xt1)==0){
    return(NA_real_)
  }

  # set the price and quantity vectors
  p0 <- xt0[[pvar]]
  p1 <- xt1[[pvar]]
  q0 <- xt0[[qvar]]
  q1 <- xt1[[qvar]]

  switch(indexMethod,
         fisher = fisher_t(p0, p1, q0, q1),
         tornqvist = tornqvist_t(p0, p1, q0, q1),
         tpd = tpd_t(p0, p1, q0, q1, xt0[[prodID]], xt1[[prodID]], biasAdjust, weights),
         walsh = walsh_t(p0, p1, q0, q1),
         jevons = jevons_t(p0, p1))

}


#' bandCache
#'
#' Wraps a function of two periods (j, k) so that each result is only
#' calculated once. Multilateral windows overlap, so the same bilateral
#' comparisons are needed by many windows. Only pairs of periods within
#' 'width' periods of each other are needed, so the results are stored in a
#' band of n x (2*width - 1) rather than a full n x n matrix.
#' @param n the number of periods
#' @param width the window length
#' @param fun the function to cache
#' @keywords internal
#' @noRd
bandCache <- function(n, width, fun){

  values <- matrix(NA_real_, nrow = n, ncol = 2*width - 1)
  done <- matrix(FALSE, nrow = n, ncol = 2*width - 1)

  function(j, k){
    col <- k - j + width
    if(!done[j, col]){
      values[j, col] <<- fun(j, k)
      done[j, col] <<- TRUE
    }
    values[j, col]
  }

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
#' @param intGEKS whether to estimate the intersection GEKS method. This method performs
#' additional product matching over the sample = "matched" option. See Lamboray and Krsinich
#' 2015 for more information.
#' @param imputePrices the type of price imputation to use for missing prices.
#' Currently only "carry" is supported to used carry-forward/carry-backward prices.
#' Default is NULL to not impute missing prices.
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
#'
#' The half and hasp splices use period (window-1)/2+1 of the window as the overlapping period.
#' If the window length is even, this is rounded down.
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
#'
#' Lamboray, C. and F. Krsinich (2015), "A Modification of the GEKS Index When
#' Product Turnover is High", Paper presented at the fourteenth Ottawa Group
#' meeting, 20-22 May 2015, Tokyo, Japan.
#'
#' @export
GEKSIndex <- function(x, pvar, qvar, pervar,indexMethod = "tornqvist", prodID,
                      sample = "matched", window = 13, splice = "mean", biasAdjust = FALSE,
                      weights = "average", intGEKS = FALSE, imputePrices = NULL){

  # normalise the case of character arguments
  indexMethod <- tolower(indexMethod)
  splice <- tolower(splice)
  weights <- tolower(weights)

  # check that only valid index methods are chosen
  if(!(indexMethod %in% c("fisher","tornqvist", "tpd", "walsh", "jevons"))){
    stop("Not a valid index number method.")
  }

  # check that only valid splice methods are chosen
  if(!(splice %in% c("mean", "window", "movement", "half", "fbew", "fbmw", "wisp", "hasp", "mean_pub"))){
    stop("Not a valid splicing method.")
  }

  # check that a valid sample type is given
  if(!(sample %in% c("matched", "unmatched", ""))){
    stop("Not a valid sample type. Please choose from matched or unmatched.")
  }

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  # check that columns are the right class
  x <- checkTypes(x, pvar, qvar, pervar)

  # check that the time period variable is continuous
  timeCheck <- isContinuous(x[[pervar]])
  if(timeCheck$result == FALSE){
    stop(paste("The time period variable is not continuous.",
               "Missing periods:", timeCheck$missing))
  }

  # check that the time period variable starts at 1
  checkStartsAtOne(x[[pervar]])

  # check that data are unique by time and product ID
  tpCheck <- checkTimeProdUnique(x, pervar, prodID)
  if(tpCheck$result == FALSE){
    stop("Products must only have one observation for each time period. If you have multiple observations on products for one or more time periods, combine this information using unitValues() or another method before calculating the price index.")
  }

  # get the number of periods
  n <- max(x[[pervar]],na.rm = TRUE)
  if(n<window){
    stop("The window length exceeds the number of periods in the data")
  }

  # apply price imputation
  if(!is.null(imputePrices)){
    switch(imputePrices,
           "carry" = {x <- imputeCarryPrices(x, pvar, qvar, pervar, prodID)},
           stop("Invalid imputePrices argument"))
  }

  # sort the dataset by time period and product ID
  x <- x[order(x[[pervar]], x[[prodID]]),]

  # find the rows for each time period, so that the data for a period can be
  # extracted without scanning the whole dataset
  rows <- periodRows(x, pervar)
  cols <- unique(c(pvar, qvar, prodID))

  # the bilateral comparisons do not depend on the window, and consecutive
  # windows share most of them, so calculate each one only once
  if(intGEKS){
    cachedIndex <- bandCache(n, window, function(j, k){
      intGEKSComparison(x, rows, cols, j, k, pvar, qvar, prodID, indexMethod,
                        biasAdjust, weights)
    })
    windowIndex <- function(periods){intGEKS_w(periods, cachedIndex)}
  } else {
    cachedIndex <- bandCache(n, window, function(j, k){
      geksBilateral(x, rows, cols, j, k, pvar, qvar, prodID, indexMethod,
                    sample, biasAdjust, weights)
    })
    windowIndex <- function(periods){GEKS_w(periods, cachedIndex)}
  }

  # initialise some matrices
  # final price index
  pGEKS <- matrix(0, nrow = n, ncol = 1)

  # set the sequence of base periods for fbew and fbmw splices
  bases <- seq(from = 1, to = n, by = window - 1)

  # first estimate a GEKS index for the first (window) observations
  tempGEK <- windowIndex(1:window)

  pGEKS[1:window,1] <- tempGEK$pgeo

  # initiate a vector of warnings for NAs
  if(length(tempGEK$naPairs) > 0){
    naWarn <- paste0("1 to ",window,": ",paste(tempGEK$naPairs, collapse = ", "),"\n")
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

      # estimate the index on the next window
      if(splice == "fbew"){
        tempGEK <- windowIndex(base:(i + window - 1))
      }
      else {
        tempGEK <- windowIndex(i:(i + window - 1))
      }
      new <- tempGEK$pgeo

      if(length(tempGEK$naPairs) > 0){
        naWarn <- paste0(naWarn, i, " to ",i+window-1,": ",
                         paste(tempGEK$naPairs, collapse = ", "), "\n")
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



#' intGEKS_w
#'
#' Function to compute the intersection GEKS index over a window.
#' This is not exposed to the user because GEKSIndex calls this
#' @param periods the time periods in the window
#' @param comparison a function of two periods (j, k) that returns the
#' intersection GEKS comparison between periods j-1 and j via period k,
#' or NA if there are no matched products
#' @keywords internal
#' @noRd
intGEKS_w <- function(periods, comparison){

  # get the window length
  window <- length(periods)

  # initialise some matrices
  # matrix of all price indices using each month as the base period
  pindices <- matrix(1, nrow = window, ncol = 1)
  pgeks <- matrix(1, nrow = window, ncol = 1)

  # initialise a vector for storing NA pair information
  naPairs <- character()

  # for every period in the window...
  for(j in 2:window){

    # for every period in the window...
    for(k in 1:window){

      pindices[k, 1] <- comparison(periods[j], periods[k])

      if(is.na(pindices[k, 1])){
        naPairs <- c(naPairs, paste0("(",j,",",k,")"))
      }

    }

    pgeks[j, 1] <- geomean(pindices, na.rm = TRUE)

  }

  # normalise to the first period
  pgeks <- pgeks/pgeks[1,1]

  # chain the period-on-period indexes
  pgeks <- cumprod(pgeks)

  return(list(pgeo = pgeks, naPairs = naPairs))
}


#' intGEKSComparison
#'
#' Calculates the intersection GEKS comparison between periods j-1 and j,
#' via period k, using the products available in all three periods.
#' @param x the data, sorted by time period and product
#' @param rows the output of periodRows(x, pervar)
#' @param cols the columns needed for the calculation
#' @param j the comparison period (the base period is j-1)
#' @param k the linking period
#' @return the comparison, or NA if there are no matched products
#' @keywords internal
#' @noRd
intGEKSComparison <- function(x, rows, cols, j, k, pvar, qvar, prodID, indexMethod,
                              biasAdjust, weights){

  # set the period j-1 = base period
  xt0 <- getPeriod(x, rows, j - 1, cols)
  # set the period j = comparison period
  xt1 <- getPeriod(x, rows, j, cols)
  # set the k period sample
  xtk <- getPeriod(x, rows, k, cols)
  intProds <- unique(xtk[[prodID]])

  # get matched sample across periods j, j-1, k
  xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
  xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
  xt1 <- xt1[xt1[[prodID]] %in% intProds,]
  xt0 <- xt0[xt0[[prodID]] %in% intProds,]
  xtk <- xtk[xtk[[prodID]] %in% unique(xt0[[prodID]]),] # only need to do this against one of xt0 or xt1

  # the comparison is NA if there are no matches
  if(nrow(xt1) == 0){
    return(NA_real_)
  }

  # set the price and quantity vectors
  p0 <- xt0[[pvar]]
  p1 <- xt1[[pvar]]
  q0 <- xt0[[qvar]]
  q1 <- xt1[[qvar]]
  pk <- xtk[[pvar]]
  qk <- xtk[[qvar]]

  # calculate the price index for 'base' period j-1 and period k
  pbIndex <- switch(indexMethod,
                    fisher = fisher_t(p0, pk, q0, qk),
                    tornqvist = tornqvist_t(p0, pk, q0, qk),
                    tpd = tpd_t(p0, pk, q0, qk, xt0[[prodID]], xtk[[prodID]], biasAdjust, weights),
                    walsh = walsh_t(p0, pk, q0, qk),
                    jevons = jevons_t(p0, pk))

  # calculate the price index for period k and comparison period j
  pcIndex <- switch(indexMethod,
                    fisher = fisher_t(pk, p1, qk, q1),
                    tornqvist = tornqvist_t(pk, p1, qk, q1),
                    tpd = tpd_t(pk, p1, qk, q1, xtk[[prodID]], xt1[[prodID]], biasAdjust, weights),
                    walsh = walsh_t(pk, p1, qk, q1),
                    jevons = jevons_t(pk, p1))

  return(pbIndex*pcIndex)

}

