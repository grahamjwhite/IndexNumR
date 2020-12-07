#' Geary-Khamis index for a window
#'
#' Calculate the GK index for a window of w periods.
#' This is not exposed to the user since it only computes
#' part of the index, and is called by GKIndex().
#' @keywords internal
#' @noRd
gk_w <- function(x,pvar,qvar,pervar,prodID, sample) {

  # total time periods
  obs <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  # if matching requested, keep only products that occur through the whole window
  if(sample == "matched"){
    x <- windowMatch(x, pervar, prodID)
  }
  else {
    # fill out the gaps from missing/new products with NAs.
    # we'll ignore them in the calcs, but it makes sure that the dimensions
    # are correct to allow calculations to proceed.
    x <- fillMissing(x, pvar, qvar, pervar, prodID, priceReplace = 0, quantityReplace = 0)
  }

  # set up some variables
  # list of time periods
  pers <- sort(unique(x[[pervar]]))
  # list of products
  prods <- sort(unique(x[[prodID]]))
  # total number of products
  n <- length(prods)

  # share of expenditure on each product in each time period
  stn <- matrix(0, nrow = obs, ncol = n)
  # quantities matrix
  qtn <- matrix(0, nrow = obs, ncol = n)
  # expenditures matrix
  etn <- matrix(0, nrow = obs, ncol = n)

  pmat <- matrix(x[[pvar]], nrow = obs, byrow = TRUE)
  qtn <- matrix(x[[qvar]], nrow = obs, byrow = TRUE)
  etn <- Reduce(`*`, list(pmat, qtn))

  # replace NAs with zeros
  etn <- replace(etn, is.na(etn), 0)
  qtn <- replace(qtn, is.na(qtn), 0)

  # calculate expenditure shares
  et <- rowSums(etn)
  for(i in 1:obs){
    stn[i,] <- etn[i,]/et[i]
  }

  # sum quantities for each product across all time
  q <- colSums(qtn)

  # formulate the inverse diagonal matrix with q down the main diagonal
  D <- solve(diag(q, nrow = n, ncol = n))

  # calculate C
  sq <- matrix(0, nrow = n, ncol = n)
  for(i in 1:obs){
    sq <- sq + stn[i,]%*%t(qtn[i,])
  }
  C <- D%*%sq

  # Compute I
  I <- diag(1, nrow = n, ncol = n)

  # [I-C] is not invertible so need to normalise
  # Make z vector
  z <- c(1, rep(0, n-1))
  # make R matrix
  R <- matrix(0, nrow = n, ncol = n)
  R[1,] <- rep(1, n)

  # Now solve for b
  b <- solve(I - C + R)%*%z

  # With b we can calculate P
  pindex <- matrix(0, nrow = obs, ncol = 1)
  for(i in 1:obs){

    temp <- x[x[[pervar]] == pers[i],]

    pindex[i,1] <- (temp[[pvar]]%*%temp[[qvar]])/(t(b)%*%temp[[qvar]])

  }

  # normalise to first period
  pindex <- pindex/pindex[1,1]

  return(pindex)

}

#' Compute the Geary-Khamis index
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param prodID A character string for the name of the product identifier
#' @param sample set to "matched" to only use products that occur
#' across all periods in a given window. Default is not to match.
#' @param window An integer specifying the length of the window.
#' @param splice the splicing method to use to extend the index. Valid methods are
#' window, movement, half or mean. The default is mean.
#' @examples
#' # compute a Geary-Khamis index with mean splicing
#' GKIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", window=11, splice = "mean")
#' @references Ivancic, L., W.E. Diewert and K.J. Fox (2011), "Scanner Data,
#' Time Aggregation and the Construction of Price Indexes", Journal of
#' Econometrics 161, 24-35.
#' @export
GKIndex <- function(x, pvar, qvar, pervar, prodID, sample = "", window, splice = "mean"){

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
  pGK <- matrix(0, nrow = n, ncol = 1)

  # first estimate a GK index for the first (window) observations
  # subset the window of data to use
  xWindow <- x[x[[pervar]] >= 1 & x[[pervar]] <= window,]

  # call gk_w on first window
  pGK[1:window,1] <- gk_w(xWindow, pvar, qvar, pervar, prodID, sample)

  # use a splicing method to compute the rest of the index
  if(n > window){
    for(i in 2:(n-window+1)){
      # set the old window
      if(i==2){
        old <- pGK[(i-1):(i+window-2),1]
      }
      else {
        old <- new
      }

      # fetch the next window of data
      xWindow <- x[x[[pervar]]>=i & x[[pervar]] < i + window,]

      # call gk_w on this window
      new <- gk_w(xWindow, pvar, qvar, pervar, prodID, sample)

      # splice the new datapoint on
      pGK[i+window-1,1] <- splice_t(pGK[i+window-2,1], old, new, method=splice)
    }
  }

  return(pGK)

}
