#' Dutot
#'
#' compute a bilateral Dutot index for a single period
#'
#' @keywords internal
dutot_t <- function(p0,p1){
  M0 <- length(p0)
  M1 <- length(p1)
  return(((1/M1)*sum(p1))/((1/M0)*sum(p0)))
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

#' Walsh index
#'
#' compute a bilateral Walsh index
#'
#' @keywords internal
walsh_t <- function(p0,p1,q0,q1){
  return(sum(p1*sqrt(q0*q1))/sum(p0*sqrt(q0*q1)))
}

#' Lloyd-Moulton index, period 0 share
#'
#' @keywords internal
lloydMoulton_t0 <- function(p0,p1,q,sigma){
  e <- sum(p0*q)
  s0 <- (p0*q)/e
  return(sum((s0*((p1/p0)^(1-sigma))))^(1/(1-sigma)))
}

#' Lloyd-Moulton index, current period share
#'
#' @keywords internal
lloydMoulton_tc <- function(p0,p1,q,sigma){
  e <- sum(p1*q)
  s1 <- (p1*q)/e
  return(sum((s1*((p1/p0)^-(1-sigma))))^(-1/(1-sigma)))
}

#' Computes a bilateral price index
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
#' harmonic, tornqvist, satovartia, walsh and CES.
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
#' @param sigma The elasticity of substitution for the CES index method.
#' @param ... this is used to pass additional parameters to the mixScaleDissimilarity
#' function.
#' @examples
#' # period-on-period Laspeyres index for the CES_sigma_2 dataset
#' priceIndex(CES_sigma_2, pvar="prices", qvar="quantities", pervar="time",
#' prodID = "prodID", indexMethod = "laspeyres")
#'
#' # chained Fisher index
#' priceIndex(CES_sigma_2, pvar="prices", qvar="quantities", pervar="time",
#' prodID = "prodID", indexMethod = "fisher", output="chained")
#'
#' # chained Tornqvist index, with linking periods chosen by the
#' # weighted log-quadratic dissimilarity measure
#' priceIndex(CES_sigma_2, pvar="prices", qvar="quantities", pervar="time",
#' prodID = "prodID", indexMethod = "tornqvist", output="chained",
#' chainMethod = "logquadratic")
#' @export
priceIndex <- function(x,pvar,qvar,pervar,indexMethod="laspeyres",prodID,
                       sample="matched",output="pop",chainMethod="pop",
                       sigma=1.0001, ...){

  # check that a valid method is chosen
  validMethods <- c("dutot","carli","jevons","harmonic","cswd","laspeyres",
                    "paasche","fisher","tornqvist","satovartia","walsh","ces")
  if(!(tolower(indexMethod) %in% validMethods)){
    stop("Not a valid index number method.")
  }

  # check that a valid output type is chosen
  validOutput <- c("chained","pop","fixedbase")
  if(!(tolower(output) %in% validOutput)){
    stop("Not a valid output type. Please choose from chained, fixedbase or pop.")
  }

  # check that the time period variable is continuous
  timeCheck <- isContinuous(x[[pervar]])
  if(timeCheck$result == FALSE){
    stop(paste("The time period variable is not continuous.",
                "Missing periods:", timeCheck$missing))
  }

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
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
    links <- maximumSimilarityLinks(similarityMatrix)
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
    # otherwise set xt1 to current period data
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
           satovartia = {plist[i,1] <- satoVartia_t(p0,p1,q0,q1)},
           walsh = {plist[i,1] <- walsh_t(p0,p1,q0,q1)},
           ces = {plist[i,1] <- lloydMoulton_t0(p0,p1,q0,sigma = sigma)}
           )

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

#' Computes a bilateral quantity index
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
#' harmonic, tornqvist, satovartia, walsh and CES.
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
#' @param sigma The elasticity of substitution for the CES index method.
#' @param ... this is used to pass additional parameters to the mixScaleDissimilarity
#' function.
#' @examples
#' # chained Fisher quantity index for the CES_sigma_2 dataset
#' quantityIndex(CES_sigma_2, pvar="prices", qvar="quantities", pervar="time",
#' prodID = "prodID", indexMethod = "fisher", output="chained")
#' @export
quantityIndex <- function(x,pvar,qvar,pervar,indexMethod="laspeyres", prodID,
                          sample="matched", output="pop", chainMethod="pop",
                          sigma=1.0001, ...){
  return(priceIndex(x, pvar=qvar, qvar=pvar, pervar = pervar, indexMethod=indexMethod,
                    prodID = prodID, sample = sample, output = output,
                    chainMethod = chainMethod, sigma = sigma, ... = ...))
}
