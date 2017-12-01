#' Computes the elasticity of substitution
#'
#' A function to estimate the elasticity of substitution
#'
#' @param x A dataframe
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param compIndex The index number with which the CES index will be equated
#' to calcuate the elasticity. Acceptable options are lloydmoulton, fisher
#' or satovartia. The lloydmoulton option equates the 'base period' lloyd-moulton
#' index with the 'current period' lloyd-moulton index.
#' @param lower lower limit to search for sigma.
#' @param upper upper limit to search for sigma.
#' @param indexType either pop to use period-on-period indexes or fixedbase
#' to use fixed base indexes.
#' @return A list with three elements: sigma (the average elasticity
#' over all time periods); allsigma (a T-1 by 1 matrix of the estimated
#' elasticities for each time period, except period one); and diff
#' (the value of the difference between the two indexes, check this is zero
#' for all time periods).
#' @examples
#' elasticity(CES_sigma_2,pvar="prices",qvar="quantities",pervar="time",
#' prodID = "prodID",indexType = "pop")
#' @export
elasticity <- function(x, pvar, qvar, pervar, prodID,
                          compIndex = "ces", lower = -20, upper = 20,
                          indexType = "pop"){

  # initialise some things
  n <- max(x[[pervar]],na.rm = TRUE)
  result <- list()
  sigmas <- matrix(0, nrow=n-1, ncol=1)
  diff <- matrix(0,nrow=n-1,ncol=1)

  # if fixed base requested, set xt0 to the first period data
  if(tolower(indexType)=="fixedbase"){
    xt0 <- x[x[[pervar]]==1,]
  }

  for(i in 2:n){
    # if pop requested then set xt0 to the previous period
    if(tolower(indexType) == "pop"){
      xt0 <- x[x[[pervar]]==i-1,]
    }

    # set xt1 to current period data
    xt1 <- x[x[[pervar]]==i,]

    # remove unmatched items
    xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
    xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]

    # set p and q
    p0 <- xt0[[pvar]]
    p1 <- xt1[[pvar]]
    q0 <- xt0[[qvar]]
    q1 <- xt1[[qvar]]

    # find the solution to (CES index - compIndex) = 0
    root <- stats::uniroot(function(x) indexDiff(x,p0,p1,q0,q1,compIndex=compIndex),
                    lower = lower, upper = upper, tol = 0.00001)

    # save result
    sigmas[i-1] <- root$root
    diff[i-1] <- root$f.root
  }

  result$sigma <- mean(sigmas)
  result$allsigma <- sigmas
  result$diff <- diff

  return(result)
}


#' Computes the difference between a CES index and a comparison index
#'
#' A function, with a root at zero, that computes the difference between
#' the CES index and a comparison index.
#' @keywords internal
indexDiff <- function(sigma,p0,p1,q0,q1,compIndex="ces"){
  lloyd0 <- lloydMoulton_t0(p0,p1,q0,sigma)
  switch (tolower(compIndex),
          ces = {diff <- lloyd0 - lloydMoulton_tc(p0,p1,q1,sigma)},
          fisher = {diff <- lloyd0 - fisher_t(p0,p1,q0,q1)},
          satovartia = {diff <- lloyd0 - satoVartia_t(p0,p1,q0,q1)}
  )
  return(diff)
}
