#' GEKS_w
#'
#' Function to compute a GEKS index over a window.
#' The window size is assumed to be the number of periods available in pervar.
#' This is not exposed to the user because GEKSIndex calls this
#' @keywords internal
GEKS_w <- function(x,pvar,qvar,pervar,indexMethod="tornqvist",prodID,
                   sample="matched"){

  # get the window length
  window <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  # initialise some matrices
  # matrix of all price indices using each month as the base period
  pindices <- matrix(0, nrow = window, ncol = window)

  # get the vector of period indices that are inside the window
  pi <- unique(x[[pervar]])

  # for every period in the window...
  for(j in 1:window){
    # set the period pi(j) = base period
    xt0 <- x[x[[pervar]] == pi[j],]
    # for every period in the window...
    for(k in 1:window){
      # set the period pi(k) = period '1'
      xt1 <- x[x[[pervar]] == pi[k],]

      # if user asked for matching, get matched samples
      if(sample=="matched"){
        xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
        xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
      }

      # set the price and quantity vectors
      p0 <- xt0[[pvar]]
      p1 <- xt1[[pvar]]
      q0 <- xt0[[qvar]]
      q1 <- xt1[[qvar]]

      # calculate the price index for 'base' period j and 'next' period k
      switch(tolower(indexMethod),
             fisher = {pindices[j,k] <- fisher_t(p0,p1,q0,q1)},
             tornqvist = {pindices[j,k] <- tornqvist_t(p0,p1,q0,q1)})
    }
  }
  # compute the geometric mean of each column of the price indices matrix
  pgeo <- apply(pindices,2,geomean)

  # normalise to the first period
  pgeo <- pgeo/pgeo[1]

  return(pgeo)
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
#' number methods are fisher or tornqvist. The default is tornqvist.
#' @param sample A character string specifying whether matching is to be performed.
#' The default is to use matching.
#' If sample=matched then any products that are not present in comparison periods
#' are removed prior to estimating the index for those periods.
#' @param window An integer specifying the length of the GEKS window.
#' @param splice A character string specifying the splicing method. Valid methods are
#' window, movement or mean. The default is mean.
#' @details The splicing methods are used to update the price index when new data become
#' available without changing prior index values. The window and movement splicing methods
#' first calculate an 'update factor' by calculating the ratio of the final index value
#' in the new GEKS window to some base period and then multiply the relevant old GEKS
#' index value by the update factor. If splice=window, the base period is the first
#' observation of the new GEKS window, and the update factor is multiplied by the
#' second observation in the old GEKS window. If splice=movement then the base period
#' is the second to last observation of the new GEKS window, and the update factor is
#' multiplied by the final observation in the old GEKS window. If splice=mean then
#' all possible values of the new index value are computed using all possible update
#' factors and then a geometric mean is computed over these possibilities to arrive
#' at the final index value.
#' @references Ivancic, L., W.E. Diewert and K.J. Fox (2011), "Scanner Data,
#' Time Aggregation and the Construction of Price Indexes", Journal of
#' Econometrics 161, 24-35.
#' @export
GEKSIndex <- function(x,pvar,qvar,pervar,indexMethod="tornqvist",prodID,
                      sample="matched",window=13,splice="mean"){

  # check that only valid index methods are chosen
  if(!(tolower(indexMethod) %in% c("fisher","tornqvist"))){
    stop("Not a valid index number method.")
  }

  # check that only valid splice methods are chosen
  if(!(tolower(splice) %in% c("mean","window","movement","half"))){
    stop("Not a valid splicing method.")
  }

  # get the number of periods
  n <- max(x[[pervar]],na.rm = TRUE)
  if(n<window){
    stop("The window length exceeds the number of periods in the data")
  }

  # initialise some matrices
  # final price index
  pGEKS <- matrix(0, nrow = n, ncol = 1)

  # first estimate a GEKS index for the first (window) observations
  # subset the window of data to use
  xWindow <- x[x[[pervar]] >= 1 & x[[pervar]] <= window,]

  # call GEKS_w on first window
  pGEKS[1:window,1] <- GEKS_w(xWindow,pvar,qvar,pervar,indexMethod,prodID,
                              sample)

  # use a splicing method to compute the rest of the index
  if(n>window){
    for(i in 2:(n-window+1)){
      # fetch the next window of data
      xWindow <- x[x[[pervar]]>=i & x[[pervar]] < i + window,]

      # call GEKS_w on this window
      tempGEKS <- GEKS_w(xWindow,pvar,qvar,pervar,indexMethod,prodID,
                         sample)

      # splice the new datapoint on
      pGEKS[i+window-1,1] <- splice_t(pGEKS[(i-1):(i+window-2),1],tempGEKS,method=splice)
    }
  }
  return(pGEKS)
}


# function to pass the correct values to splice helper functions
splice_t <- function(oldGEK,newGEK,method="mean"){
  switch(method,
         movement = {pt <- movementSplice(oldGEK[length(oldGEK)],newGEK)},
         window = {pt <- windowSplice(oldGEK[2],newGEK)},
         mean = {pt <- meanSplice(oldGEK,newGEK)}
  )
  return(pt)
}

# functions to splice datapoints onto a GEKS index
# movement splice using the final two periods of the new GEKS index
movementSplice <- function(x,NewGEK){
  spliceFactor <- NewGEK[length(NewGEK)]/NewGEK[length(NewGEK)-1]
  return(x*spliceFactor)
}

# window splice using the first and last observations of the new GEKS
windowSplice <- function(x,NewGEK){
  spliceFactor <- NewGEK[length(NewGEK)]/NewGEK[1]
  return(x*spliceFactor)
}

# mean splicing using the geometric mean of all overlapping periods
meanSplice <- function(oldGEK,newGEK){
  w = length(newGEK)
  pvector <- matrix(0,nrow=w-1,ncol=1)

  for(l in 1:(w-1)){
    pvector[l,1] <- oldGEK[l+1]*(newGEK[w]/newGEK[l])
  }
  return(geomean(pvector))
}
