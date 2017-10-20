#' Dutot
#'
#' compute a bilateral Dutot index for a single period
#'
#' @keywords internal
dutot_t <- function(p0,p1){
  M0 <- length(p0)
  M1 <- length(p1)
  return(((1/M0)*sum(p0))/((1/M1)*sum(p1)))
}

#' Carli
#'
#' compute a bilateral Carli index for a single period
#'
#' @keywords internal
carli_t <- function(p0,p1){
  return((1/length(p0))*sum(p1/p0))
}

#' Jevons
#'
#' compute a bilateral Jevons index for a single period
#'
#' @keywords internal
jevons_t <- function(p0,p1){
  return(prod((p1/p0)^(1/length(p0))))
}

#' Harmonic mean
#'
#' compute a bilateral Harmonic mean index for a single period
#'
#' @keywords internal
harmonic_t <- function(p0,p1){
  return((1/length(p0)*sum((p1/p0)^-1))^-1)
}

#' CSWD
#'
#'compute a bilateral CSWD index for a single period
#'
#' @keywords internal
cswd_t <- function(p0,p1){
  return(sqrt((carli_t(p0,p1)*harmonic_t(p0,p1))))
}

#' Fixed-basket indices - Laspeyres (q=q0), Paasche (q=q1), Lowe (q=qb)
#'
#' compute a bilateral fixed base index for a single period
#'
#' @keywords internal
fixed_t <- function(p0,p1,q){
  return(sum(p1*q)/sum(p0*q))
}

#' fischer_t
#'
#' compute a bilateral Fisher index for a single period
#'
#' @keywords internal
fisher_t <- function(p0,p1,q0,q1){
  las <- fixed_t(p0,p1,q0)
  pas <- fixed_t(p0,p1,q1)
  return(sqrt((las*pas)))
}

#' tornqvist_T
#'
#' compute a bilateral Tornqvist index for a single period
#'
#' @keywords internal
tornqvist_t <- function(p0,p1,q0,q1){
  exp0 <- sum(p0*q0)
  exp1 <- sum(p1*q1)
  s0 <- (p0*q0)/exp0
  s1 <- (p1*q1)/exp1
  return(prod((p1/p0)^(0.5*(s0+s1))))
}

#' priceIndex
#'
#' A function to compute a price index given data on product prices over time
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
#' @param output A character string specifying whether a chained, fixed base or
#' period-on-period price index numbers should be returned.
#' @export
priceIndex <- function(x,pvar,qvar,pervar,indexMethod="laspeyres",prodID,
                       sample="matched",output="pop"){

  validMethods <- c("dutot","carli","jevons","harmonic","cswd","laspeyres",
                    "paasche","fisher","tornqvist")
  if(!(tolower(indexMethod) %in% validMethods)){
    stop("Not a valid index number method.")
  }

  n <- max(x[[pervar]],na.rm = TRUE)
  pmat <- matrix(1, nrow = n, ncol = 1)

  if(tolower(output)=="fixedbase"){
    xt0 <- x[x[[pervar]]==1,]
  }

  for(i in 2:n){
    if(!(tolower(output)=="fixedbase")){
      xt0 <- x[x[[pervar]]==i-1,]
    }
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
           dutot = {pmat[i,1] <- dutot_t(p0,p1)},
           carli = {pmat[i,1] <- carli_t(p0,p1)},
           jevons = {pmat[i,1] <- jevons_t(p0,p1)},
           harmonic = {pmat[i,1] <- harmonic_t(p0,p1)},
           cswd = {pmat[i,1] <- cswd_t(p0,p1)},
           laspeyres = {pmat[i,1] <- fixed_t(p0,p1,q0)},
           paasche = {pmat[i,1] <- fixed_t(p0,p1,q1)},
           fisher = {pmat[i,1] <- fisher_t(p0,p1,q0,q1)},
           tornqvist = {pmat[i,1] <- tornqvist_t(p0,p1,q0,q1)})
  }

  if(output=="chained"){
    result <- apply(pmat,2,cumprod)
  }
  else{
    result <- pmat
  }

  return(result)
}

#' quantityIndex
#'
#' A function to compute a price index given data on product prices over time
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
#' @param output A character string specifying whether a chained, fixed base or
#' period-on-period price index numbers should be returned.
#' @export
quantityIndex <- function(x,pvar,qvar,pervar,indexMethod="laspeyres",prodID,
                          sample="matched",output="pop"){

  validMethods <- c("dutot","carli","jevons","harmonic","cswd","laspeyres",
                    "paasche","fisher","tornqvist")
  if(!(tolower(indexMethod) %in% validMethods)){
    stop("Not a valid index number method.")
  }

  n <- max(x[[pervar]],na.rm = TRUE)
  plist <- matrix(1, nrow = n, ncol = 1)

  if(tolower(output)=="fixedbase"){
    xt0 <- x[x[[pervar]]==1,]
  }

  for(i in 2:n){
    if(!(tolower(output)=="fixedbase")){
      xt0 <- x[x[[pervar]]==i-1,]
    }
    xt1 <- x[x[[pervar]]==i,]

    if(sample=="matched"){
      xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
      xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
    }

    p0 <- xt0[[pvar]]
    p1 <- xt1[[pvar]]
    q0 <- xt0[[qvar]]
    q1 <- xt1[[qvar]]

    switch(tolower(indexMethod),
           dutot = {plist[i,1] <- dutot_t(q0,q1)},
           carli = {plist[i,1] <- carli_t(q0,q1)},
           jevons = {plist[i,1] <- jevons_t(q0,q1)},
           harmonic = {plist[i,1] <- harmonic_t(q0,q1)},
           cswd = {plist[i,1] <- cswd_t(q0,q1)},
           laspeyres = {plist[i,1] <- fixed_t(q0,q1,p0)},
           paasche = {plist[i,1] <- fixed_t(q0,q1,p1)},
           fisher = {plist[i,1] <- fisher_t(q0,q1,p0,p1)},
           tornqvist = {plist[i,1] <- tornqvist_t(q0,q1,p0,p1)})
  }

  if(output=="chained"){
    result <- apply(plist,2,cumprod)
  }
  else{
    result <- plist
  }

  return(result)
}
