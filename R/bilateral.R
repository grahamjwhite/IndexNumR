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

#' fisher_t
#'
#' compute a bilateral Fisher index for a single period
#'
#' @keywords internal
fisher_t <- function(p0,p1,q0,q1){
  las <- fixed_t(p0,p1,q0)
  pas <- fixed_t(p0,p1,q1)
  return(sqrt((las*pas)))
}

#' tornqvist_t
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

#' Sato-Vartia index
#'
#' compute a bilateral Sato-Vartia index
#' @keywords internal
satoVartia_t <- function(p0,p1,q0,q1){
  exp0 <- sum(p0*q0)
  exp1 <- sum(p1*q1)
  s0 <- (p0*q0)/exp0
  s1 <- (p1*q1)/exp1
  wstar <- (s1-s0)/(log(s1)-log(s0))
  w <- wstar/sum(wstar)
  return(exp(sum(w*log(p1/p0))))
}

#' priceIndex
#'
#' A function to compute a price index given data on products over time
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
#' harmonic, tornqvist and satovartia.
#' @param sample A character string specifying whether a matched sample
#' should be used.
#' @param output A character string specifying whether a chained, fixed base or
#' period-on-period price index numbers should be returned. Default is period-on-period.
#' @param chainMethod A character string specifying the method of chain linking
#' to use if the output option is set to "chained".
#' Valid options are "pop" for period-on-period, and similarity chain linked
#' options "plspread" for the Paasche-Laspeyres spread, "asymplinear" for
#' weighted asymptotically linear, "logquadratic" for the weighted log-quadratic,
#' and "mixScale" for the mix, scale or absolute dissimilarity measures.
#' The default is period-on-period. Additional parameters can be passed to the
#' mixScaleDissimilarity function using ...
#' @param ... this is used to pass additional parameters to the mixScaleDissimilarity
#' function.
#' @export
priceIndex <- function(x,pvar,qvar,pervar,indexMethod="laspeyres",prodID,
                       sample="matched",output="pop",chainMethod="pop",...){

  validMethods <- c("dutot","carli","jevons","harmonic","cswd","laspeyres",
                    "paasche","fisher","tornqvist","satovartia")
  if(!(tolower(indexMethod) %in% validMethods)){
    stop("Not a valid index number method.")
  }

  # initialise some things
  n <- max(x[[pervar]],na.rm = TRUE)
  plist <- matrix(1, nrow = n, ncol = 1)

  # if similarity chaining was requested, get the similarity measure
  if(tolower(output)=="chained" & !(tolower(chainMethod)=="pop")){
    switch(tolower(chainMethod),
           plspread = {similarityMatrix <- relativeDissimilarity(x,pvar=pvar,qvar=qvar,
                                                                 pervar=pervar,prodID=prodID,
                                                                 similarityMethod = "plspread")},
           logquadratic = {similarityMatrix <- relativeDissimilarity(x,pvar=pvar,qvar=qvar,
                                                                     pervar=pervar,prodID=prodID,
                                                                     similarityMethod = "logquadratic")},
           asymplinear = {similarityMatrix <- relativeDissimilarity(x,pvar=pvar,qvar=qvar,
                                                                    pervar=pervar,prodID=prodID,
                                                                    similarityMethod = "asymplinear")},
           mixscale = {similarityMatrix <- mixScaleDissimilarity(x,pvar=pvar,qvar=qvar,
                                                                 pervar=pervar,prodID=prodID,
                                                                 ...)})
    # use the similarity matrix to compute links
    links <- maximiumSimilarityLinks(similarityMatrix)
  }

  # if fixed base requested, set xt0 to the first period data
  if(tolower(output)=="fixedbase"){
    xt0 <- x[x[[pervar]]==1,]
  }

  for(i in 2:n){
    # if chained or period-on-period requested then set xt0
    # to the previous period
    if(tolower(output) == "chained" & tolower(chainMethod)=="pop" |
       tolower(output) == "pop"){
      xt0 <- x[x[[pervar]]==i-1,]
    }
    # if similarity linking requested set xt0 to link period
    else if(tolower(output) == "chained" & !(tolower(chainMethod) == "pop")){
      xt0 <- x[x[[pervar]]==links[links$xt==i,2],]
    }
    # set xt1 to current period data
    xt1 <- x[x[[pervar]]==i,]

    # if matching requested then remove unmatched items
    if(sample=="matched"){
      xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
      xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
    }

    # set p and q
    p0 <- xt0[[pvar]]
    p1 <- xt1[[pvar]]
    q0 <- xt0[[qvar]]
    q1 <- xt1[[qvar]]

    # compute the index
    switch(tolower(indexMethod),
           dutot = {plist[i,1] <- dutot_t(p0,p1)},
           carli = {plist[i,1] <- carli_t(p0,p1)},
           jevons = {plist[i,1] <- jevons_t(p0,p1)},
           harmonic = {plist[i,1] <- harmonic_t(p0,p1)},
           cswd = {plist[i,1] <- cswd_t(p0,p1)},
           laspeyres = {plist[i,1] <- fixed_t(p0,p1,q0)},
           paasche = {plist[i,1] <- fixed_t(p0,p1,q1)},
           fisher = {plist[i,1] <- fisher_t(p0,p1,q0,q1)},
           tornqvist = {plist[i,1] <- tornqvist_t(p0,p1,q0,q1)},
           satovartia = {plist[i,1] <- satoVartia_t(p0,p1,q0,q1)})

    # if similarity chain linking then multiply the index by the link period index
    if(tolower(output) == "chained" & !(tolower(chainMethod) == "pop")){
      plist[i,1] = plist[i,1]*plist[links[links$xt==i,2],1]
    }
  }

  if(tolower(output) == "chained" & tolower(chainMethod)=="pop"){
    result <- apply(plist,2,cumprod)
  }
  else{
    result <- plist
  }

  return(result)
}

#' quantityIndex
#'
#' A function to compute a quantity index given data on products over time
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
#' harmonic, tornqvist and satovartia.
#' @param sample A character string specifying whether a matched sample
#' should be used.
#' @param output A character string specifying whether a chained, fixed base or
#' period-on-period price index numbers should be returned.
#' @param chainMethod A character string specifying the method of chain linking
#' to use if the output option is set to "chained".
#' Valid options are "pop" for period-on-period, and similarity chain linked
#' options "plspread" for the Paasche-Laspeyres spread, "asymplinear" for
#' weighted asymptotically linear, "logquadratic" for the weighted log-quadratic,
#' and "mixScale" for the mix, scale or absolute dissimilarity measures.
#' The default is period-on-period. Additional parameters can be passed to the
#' mixScaleDissimilarity function using ...
#' @param ... this is used to pass additional parameters to the mixScaleDissimilarity
#' function.
#' @export
quantityIndex <- function(x,pvar,qvar,pervar,indexMethod="laspeyres", prodID,
                          sample="matched", output="pop", chainMethod="pop", ...){
  return(priceIndex(x, pvar=qvar, qvar=pvar, pervar = pervar, indexMethod=indexMethod,
                    prodID = prodID, sample = sample, output = output,
                    chainMethod = chainMethod, ... = ...))
}
