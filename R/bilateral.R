################ IndexNumR ##############
# File: bilateral.R                     #
# Computes bilateral index numbers      #
# Author: Graham White                  #
# g.white@student.unsw.gov.au           #
# Written: 15/09/2017                   #
# Updated:                              #
#########################################

# functions to compute elementary indexes P(p0,p1) for a single time
# period. These are not exposed to the user.

# Dutot
dutot_t <- function(p0,p1){
  M0 <- length(p0)
  M1 <- length(p1)
  return(((1/M0)*sum(p0))/((1/M1)*sum(p1)))
}

# Carli
carli_t <- function(p0,p1){
  return((1/length(p0))*sum(p1/p0))
}

# Jevons
jevons_t <- function(p0,p1){
  return(prod((p1/p0)^(1/length(p0))))
}

# Harmonic mean
harmonic_t <- function(p0,p1){
  return((1/length(p0)*sum((p1/p0)^-1))^-1)
}

# CSWD
cswd_t <- function(p0,p1){
  return(sqrt((carli_t(p0,p1)*harmonic_t(p0,p1))))
}

# Fixed-basket indices - Laspeyres (q=q0), Paasche (q=q1), Lowe (q=qb)
fixed_t <- function(p0,p1,q){
  return(sum(p1*q)/sum(p0*q))
}

# Fischer
fisher_t <- function(p0,p1,q0,q1){
  las <- fixed_t(p0,p1,q0)
  pas <- fixed_t(p0,p1,q1)
  return(sqrt((las*pas)))
}

# Tornqvist
tornqvist_t <- function(p0,p1,q0,q1){
  exp0 <- sum(p0*q0)
  exp1 <- sum(p1*q1)
  s0 <- (p0*q0)/exp0
  s1 <- (p1*q1)/exp1
  return(prod((p1/p0)^(0.5*(s0+s1))))
}

# function for the geometric mean of a vector
geomean <- function(x){
  return(exp(mean(log(x))))
}

# Function to compute a price index for all
# time periods using above index number formulae

#' priceIndex
#'
#' A function to compute a price index given data over product prices over time
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
#' number methods are dutot, carli, jevons, laspeyres, paasche, fisher, cswd,
#' harmonic and tornqvist.
#' @param sample A character string specifying whether a matched sample
#' should be used.
#' @param output A character string specifying whether a chained index or
#' period-on-period price index numbers should be returned.
#' @keywords price index number
#' @export
priceIndex <- function(x,pvar,qvar,pervar,indexMethod="laspeyres",prodID,
                       sample="matched",output="pop"){

  validMethods <- c("dutot","carli","jevons","harmonic","cswd","laspeyres",
                    "paasche","fisher","tornqvist")
  if(!(tolower(indexMethod) %in% validMethods)){
    stop("Not a valid index number method.")
  }

  n <- max(x[[pervar]],na.rm = TRUE)
  plist <- matrix(1, nrow = n, ncol = 1)

  for(i in 2:n){
    xt0 <- x[x[[pervar]]==i-1,]
    xt1 <- x[x[[pervar]]==i,]

    if(sample=="matched"){
      xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
      xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
    }

    p0 <- xt0[[pvar]]
    p1 <- xt1[[pvar]]
    q0 <- xt0[[qvar]]
    q1 <- xt1[[qvar]]

    if(!(tolower(indexMethod=="dutot")) & length(p0)!=length(p1)){
      stop("Price vectors p1 and p0 are of different length. Only the
           Dutot index can computed, or use option sample=matched.")
    }

    switch(tolower(indexMethod),
           dutot = {plist[i,1] <- dutot_t(p0,p1)},
           carli = {plist[i,1] <- carli_t(p0,p1)},
           jevons = {plist[i,1] <- jevons_t(p0,p1)},
           harmonic = {plist[i,1] <- harmonic_t(p0,p1)},
           cswd = {plist[i,1] <- cswd_t(p0,p1)},
           laspeyres = {plist[i,1] <- fixed_t(p0,p1,q0)},
           paasche = {plist[i,1] <- fixed_t(p0,p1,q1)},
           fisher = {plist[i,1] <- fisher_t(p0,p1,q0,q1)},
           tornqvist = {plist[i,1] <- tornqvist_t(p0,p1,q0,q1)})
  }

  if(output=="chained"){
    result <- apply(plist,2,cumprod)
  }

  return(result)
}
