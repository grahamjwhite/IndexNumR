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


#' Dutot
#'
#' compute a bilateral Dutot index for a single period
#'
#' @keywords internal
#' @noRd
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
#' @noRd
carli_t <- function(p0,p1){
  return((1/length(p0))*sum(p1/p0))
}

#' Jevons
#'
#' compute a bilateral Jevons index for a single period
#'
#' @keywords internal
#' @noRd
jevons_t <- function(p0,p1){
  return(prod((p1/p0)^(1/length(p0))))
}

#' Harmonic mean
#'
#' compute a bilateral Harmonic mean index for a single period
#'
#' @keywords internal
#' @noRd
harmonic_t <- function(p0,p1){
  return((1/length(p0)*sum((p1/p0)^-1))^-1)
}

#' CSWD
#'
#'compute a bilateral CSWD index for a single period
#'
#' @keywords internal
#' @noRd
cswd_t <- function(p0,p1){
  return(sqrt((carli_t(p0,p1)*harmonic_t(p0,p1))))
}

#' Fixed-basket indices - Laspeyres (q=q0), Paasche (q=q1), Lowe (q=qb)
#'
#' compute a bilateral fixed base index for a single period
#'
#' @keywords internal
#' @noRd
fixed_t <- function(p0,p1,q){
  return(sum(p1*q)/sum(p0*q))
}

#' fisher_t
#'
#' compute a bilateral Fisher index for a single period
#'
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
walsh_t <- function(p0,p1,q0,q1){
  return(sum(p1*sqrt(q0*q1))/sum(p0*sqrt(q0*q1)))
}

#' Lloyd-Moulton index, period 0 share
#'
#' @keywords internal
#' @noRd
lloydMoulton_t0 <- function(p0,p1,q,sigma){
  e <- sum(p0*q)
  s0 <- (p0*q)/e
  return(sum((s0*((p1/p0)^(1-sigma))))^(1/(1-sigma)))
}

#' Lloyd-Moulton index, current period share
#'
#' @keywords internal
#' @noRd
lloydMoulton_tc <- function(p0,p1,q,sigma){
  e <- sum(p1*q)
  s1 <- (p1*q)/e
  return(sum((s1*((p1/p0)^-(1-sigma))))^(-1/(1-sigma)))
}

#' Geometric laspeyres index
#'
#' @keywords internal
#' @noRd
geomLaspeyres_t <- function(p0, p1, q0, q1){
  exp0 <- sum(p0*q0)
  s0 <- (p0*q0)/exp0
  return(prod((p1/p0)^s0))
}

#' Geometric paasche index
#'
#' @keywords internal
#' @noRd
geomPaasche_t <- function(p0, p1, q0, q1){
  exp1 <- sum(p1*q1)
  s1 <- (p1*q1)/exp1
  return(prod((p1/p0)^s1))
}

#' time product dummy
#'
#' @keywords internal
#' @noRd
tpd_t <- function(p0, p1, q0, q1, prodID0, prodID1, biasAdjust, weights){

  exp0 <- sum(p0*q0)
  exp1 <- sum(p1*q1)

  switch(weights,
         "shares" = {s0 <- (p0*q0)/exp0
                     s1 <- (p1*q1)/exp1},
         "average" = {s0Initial <- (p0*q0)/exp0
                     s1Initial <- (p1*q1)/exp1

                     df <- merge(data.frame(prodID = prodID0, s0 = s0Initial),
                                 data.frame(prodID = prodID1, s1 = s1Initial),
                                 all = TRUE)
                     df$average <- 0.5*(ifelse(is.na(df$s0), 0, df$s0) + ifelse(is.na(df$s1), 0, df$s1))

                     s0 <- df$average[!is.na(df$s0)]
                     s1 <- df$average[!is.na(df$s1)]

                     },
         "unweighted" = {s0 <- rep(NA, length(p0))
                         s1 <- rep(NA, length(p1))}
  )

  df1 <- data.frame(lnP = log(p1),
                    D = factor(rep("1", length(p1)), levels = c("0", "1")),
                    s = s1,
                    product = as.factor(prodID1))

  df0 <- data.frame(lnP = log(p0),
                    D = factor(rep("0", length(p0)), levels = c("0", "1")),
                    s = s0,
                    product = as.factor(prodID0))

  regData <- rbind(df0, df1)

  if(weights == "unweighted"){
    reg <- stats::lm(lnP ~ D + product, data = regData)
  } else {
    reg <- stats::lm(lnP ~ D + product, weights = s, data = regData)
  }

  if(biasAdjust){
    coeffs <- kennedyBeta(reg)
  }
  else {
    coeffs <- stats::coef(reg)
  }

  b <- coeffs[which(names(coeffs) == "D1")]

  return(exp(b))
}

#' Geary-Khamis bilateral
#'
#' @keywords internal
#' @noRd
gk_t <- function(p0, p1, q0, q1){

  h <- 2/(1/q0+1/q1)
  return(sum(p1*h)/sum(p0*h))

}

#' Drobish bilateral index
#'
#' @keywords internal
#' @noRd
dorbish_t <- function(p0, p1, q0, q1){

  return((fixed_t(p0,p1,q0) + fixed_t(p0,p1,q1))/2)

}

#' Stuvel bilateral index
#'
#' @keywords internal
#' @noRd
stuvel_t <- function(p0, p1, q0, q1){

  exp0 <- sum(p0*q0)
  exp1 <- sum(p1*q1)

  PL <- fixed_t(p0, p1, q0) # laspeyres price index
  QL <- fixed_t(q0, q1, p0) # laspeyres quantity index

  A <- 0.5*(PL-QL)

  return(A + (A^2 + exp1/exp0)^0.5)

}

#' Marshall-Edgeworth bilateral index
#'
#' @keywords internal
#' @noRd
marshallEdgeworth_t <- function(p0, p1, q0, q1){

  return(sum(p1*(q0+q1))/sum(p0*(q0+q1)))

}

#' Palgrave bilateral index
#'
#' @keywords internal
#' @noRd
palgrave_t <- function(p0, p1, q1){

  exp1 <- sum(p1*q1)
  s1 <- (p1*q1)/exp1

  return(sum(s1*(p1/p0)))

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
#' harmonic, tornqvist, satovartia, walsh, CES, geomLaspeyres, geomPaasche, tpd and
#' Geary-Khamis.
#' @param sample A character string specifying whether a matched sample
#' should be used.
#' @param output A character string specifying whether a chained (output="chained")
#' , fixed base (output="fixedbase") or period-on-period (output="pop")
#' price index numbers should be returned. Default is period-on-period.
#' @param chainMethod A character string specifying the method of chain linking
#' to use if the output option is set to "chained".
#' Valid options are "pop" for period-on-period, and similarity chain linked
#' options "plspread" for the Paasche-Laspeyres spread, "asymplinear" for
#' weighted asymptotically linear, "logquadratic" for the weighted log-quadratic,
#' and "mixScale" for the mix, scale or absolute dissimilarity measures.
#' The default is period-on-period. Additional parameters can be passed to the
#' mixScaleDissimilarity function using \code{...}
#' @param sigma The elasticity of substitution for the CES index method.
#' @param basePeriod The period to be used as the base when 'fixedbase' output is
#' chosen. Default is 1 (the first period).
#' @param biasAdjust whether to adjust for bias in the coefficients in the bilateral
#' TPD index. The default is TRUE.
#' @param weights the type of weighting for the bilateral TPD index. Options are
#' "unweighted" to use ordinary least squares, "shares" to use weighted least squares
#' with expenditure share weights, and "average" to use weighted least squares
#' with the average of the expenditure shares over the two periods.
#' @param loweBase the period from which quantities are taken for the lowe index.
#' The default is period 1.
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
priceIndex <- function(x, pvar, qvar, pervar, indexMethod = "laspeyres", prodID,
                       sample = "matched", output = "pop", chainMethod = "pop",
                       sigma = 1.0001, basePeriod = 1, biasAdjust = TRUE,
                       weights = "average", loweBase = 1, ...){

  # check that a valid method is chosen
  validMethods <- c("dutot","carli","jevons","harmonic","cswd","laspeyres",
                    "paasche","fisher","tornqvist","satovartia","walsh","ces",
                    "geomlaspeyres", "geompaasche", "tpd", "gk", "dorbish",
                    "stuvel", "marshalledgeworth", "palgrave", "lowe")

  if(!(tolower(indexMethod) %in% validMethods)){
    stop("Not a valid index number method.")
  }

  # check that a valid output type is chosen
  validOutput <- c("chained","pop","fixedbase")
  if(!(tolower(output) %in% validOutput)){
    stop("Not a valid output type. Please choose from chained, fixedbase or pop.")
  }

  # check that valid weights are given
  validWeights <- c("unweighted", "average", "shares")
  if(!(tolower(weights) %in% validWeights)){
    stop("Not a valid weight type. Please choose from unweighted, shares or average.")
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
  plist <- matrix(1, nrow = n, ncol = 1)
  naElements <- character()

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
  if(tolower(output) == "fixedbase"){
    xt0 <- x[x[[pervar]] == basePeriod,]
  }

  # if lowe index method chosen then set xtb to the loweBase period
  if(tolower(indexMethod) == "lowe"){
    xtb <- x[x[[pervar]] == loweBase,]
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

      # remove unmatched products
      xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
      xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]

      # because the base period can differ from period 1 and 0 for lowe index
      if(tolower(indexMethod) == "lowe"){
        xt1 <- xt1[xt1[[prodID]] %in% unique(xtb[[prodID]]),]
        xt0 <- xt0[xt0[[prodID]] %in% unique(xtb[[prodID]]),]

        # for xtb we only need to intersect with one of xt0 or xt1 because they are the same set
        xtb <- xtb[xtb[[prodID]] %in% unique(xt1[[prodID]]),]
      }

    }

    # set the price index element to NA if there are no matches
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

      if(tolower(indexMethod == "lowe")) qb <- xtb[[qvar]]

      # compute the index
      switch(tolower(indexMethod),
             dutot = {plist[i,1] <- dutot_t(p0,p1)},
             carli = {plist[i,1] <- carli_t(p0,p1)},
             jevons = {plist[i,1] <- jevons_t(p0,p1)},
             harmonic = {plist[i,1] <- harmonic_t(p0,p1)},
             cswd = {plist[i,1] <- cswd_t(p0,p1)},
             laspeyres = {plist[i,1] <- fixed_t(p0,p1,q0)},
             paasche = {plist[i,1] <- fixed_t(p0,p1,q1)},
             lowe = {plist[i,1] <- fixed_t(p0,p1,qb)},
             fisher = {plist[i,1] <- fisher_t(p0,p1,q0,q1)},
             tornqvist = {plist[i,1] <- tornqvist_t(p0,p1,q0,q1)},
             satovartia = {plist[i,1] <- satoVartia_t(p0,p1,q0,q1)},
             walsh = {plist[i,1] <- walsh_t(p0,p1,q0,q1)},
             ces = {plist[i,1] <- lloydMoulton_t0(p0,p1,q0,sigma = sigma)},
             geomlaspeyres = {plist[i,1] <- geomLaspeyres_t(p0, p1, q0, q1)},
             geompaasche = {plist[i,1] <- geomPaasche_t(p0, p1, q0, q1)},
             tpd = {plist[i,1] <- tpd_t(p0, p1, q0, q1, xt0[[prodID]], xt1[[prodID]], biasAdjust, weights)},
             gk = {plist[i,1] <- gk_t(p0, p1, q0, q1)},
             drobish = {plist[i,1] <- drobish_t(p0, p1, q0, q1)},
             stuvel = {plist[i,1] <- stuvel_t(p0, p1, q0, q1)},
             marshalledgeworth = {plist[i,1] <- marshallEdgeworth_t(p0, p1, q0, q1)},
             palgrave = {plist[i,1] <- palgrave_t(p0, p1, q1)}
      )

      # if similarity chain linking then multiply the index by the link period index
      if(tolower(output) == "chained" & !(tolower(chainMethod) == "pop")){
        plist[i,1] = plist[i,1]*plist[links[links$xt==i,2],1]
      }
    }
  }

  if(tolower(output) == "chained" & tolower(chainMethod)=="pop"){
    result <- apply(plist,2,cumprod)
  }
  else{
    result <- plist
  }

  if(length(naElements)>0){
    warning(paste0("The following elements of the index were set to NA because there were no matched products in the two comparison periods: ", naElements))
  }

  return(result)
}

#' Computes a bilateral quantity index
#'
#' A function to compute a quantity index given data on products over time
#'
#' @inheritParams priceIndex
#' @examples
#' # chained Fisher quantity index for the CES_sigma_2 dataset
#' quantityIndex(CES_sigma_2, pvar="prices", qvar="quantities", pervar="time",
#' prodID = "prodID", indexMethod = "fisher", output="chained")
#' @export
quantityIndex <- function(x, pvar, qvar, pervar, indexMethod = "laspeyres", prodID,
                          sample = "matched", output = "pop", chainMethod = "pop",
                          sigma = 1.0001, basePeriod = 1, biasAdjust = TRUE, weights = "average", ...){
  return(priceIndex(x,
                    pvar = qvar,
                    qvar = pvar,
                    pervar = pervar,
                    indexMethod = indexMethod,
                    prodID = prodID,
                    sample = sample,
                    output = output,
                    chainMethod = chainMethod,
                    sigma = sigma,
                    basePeriod = basePeriod,
                    biasAdjust = biasAdjust,
                    weights = weights,
                    ... = ...))
}
