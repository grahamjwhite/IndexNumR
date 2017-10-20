#' relativeDissimilarity
#'
#' A function to compute the relative price dissimilarity
#' between two vectors of prices.
#'
#' @param x A dataframe containing price, quantities, a time
#' period index and a product identifier.
#' @param pvar A string identifying the price variable.
#' @param qvar A string identifying the quantity variable.
#' @param pervar A string identifying the time index variable.
#' @param prodID A string identifying the product ID.
#' @param indexMethod A string identifying the index method to
#' use in the calculation. Not relevent for similarityMethod = PLSpread.
#' Supported methods are fisher and tornqvist. Default is Fisher.
#' @param similarityMethod A string specifying the formula for calculating
#' the relative dissimilarity. Valid options are logQuadratic, asymplinear
#' and PLSpread. Default is the log quadratic.
#' @return A matrix of dissimilarity measures.
#' The first two columns are the possible combinations of bilateral
#' comparisons and the third column is the dissimilarity measure.
#' @export
relativeDissimilarity <- function(x, pvar, qvar, pervar, prodID,
                                  indexMethod="fisher",
                                  similarityMethod="logquadratic"){
  if(!(tolower(indexMethod) %in% c("fisher","tornqvist"))){
    stop("Not a valid index number method.")
  }

  if(!(tolower(similarityMethod) %in% c("plspread","logquadratic",
                                        "asymplinear"))){
    stop("Not a valid similarity method.")
  }

  # get a list of possible combinations of time periods
  n <- max(x[[pervar]])
  c <- utils::combn(n,2)

  # initialise a results matrix
  res <- matrix(0, nrow=ncol(c), ncol=3)
  res[,1:2] <- t(c)

  for(i in 1:ncol(c)){
    xt0 <- x[x[[pervar]]==c[1,i],]
    xt1 <- x[x[[pervar]]==c[2,i],]

    # get a matched sample
    xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
    xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]

    # get price and quantity vectors
    p0 <- xt0[[pvar]]
    p1 <- xt1[[pvar]]
    q0 <- xt0[[qvar]]
    q1 <- xt1[[qvar]]

    if(tolower(similarityMethod) == "plspread"){
      # calculate Paasche
      p <- fixed_t(p0,p1,q1)
      # calculate Laspeyres
      l <- fixed_t(p0,p1,q0)
      # PL spread
      res[i,3] <- abs(log(l/p))
    }
    else{
      # calculate P(p0,p1)
      switch(tolower(indexMethod),
             fisher = {p <- fisher_t(p0,p1,q0,q1)},
             tornqvist = {p<- tornqvist_t(p0,p1,q0,q1)})

      # calculate expenditure shares
      s0 <- (p0*q0)/sum(p0*q0)
      s1 <- (p1*q1)/sum(p1*q1)

      # calculate dissimilarity
      switch(tolower(similarityMethod),
             logquadratic = {res[i,3] <- sum(0.5*(s0+s1)*(log(p1/(p*p0)))^2)},
             asymplinear = {res[i,3] <- sum(0.5*(s0+s1)*(p1/(p*p0)+(p0*p)/p1-2))})
    }
  }
  colnames(res) <- c("period_i","period_j","dissimilarity")
  return(res)
}

#' mixScaleDissimilarity
#'
#' Fox, Hill and Diewert 2004 vector dissimilarity
#' @param x A dataframe
#' @param pvar string identifying the price variable in x
#' @param qvar string identifying the quantity variable in x
#' @param pervar string identifying the time period variable in x
#' @param prodID string identifying the product id variable in x
#' @param measure choice of dissimilarity measure. Valid options
#' are mix, scale or absolute.
#' @return A matrix where the first two columns are the possible combinations
#' of periods and the third column is the dissimilarity measure.
#' @export
mixScaleDissimilarity <- function(x, pvar, qvar, prodID, pervar,
                                  measure="absolute"){

  # find number of combinations
  n <- max(x[[pervar]])
  cn <- utils::combn(n,2)

  # initialise results matrix
  result <- matrix(0, nrow=ncol(cn), ncol=3)

  # for each combination of time periods...
  for(i in 1:ncol(cn)){

    # get the i and j vectors
    xi <- x[x[[pervar]]==cn[1,i],]
    xj <- x[x[[pervar]]==cn[2,i],]

    # get a matched sample
    xj <- xj[xj[[prodID]] %in% unique(xi[[prodID]]),]
    xi <- xi[xi[[prodID]] %in% unique(xj[[prodID]]),]

    # get price and quantity vectors
    pi <- xi[[pvar]]
    pj <- xj[[pvar]]
    qi <- xi[[qvar]]
    qj <- xj[[qvar]]

    # stack p and q into one vector
    xstack <- c(pi,qi)
    ystack <- c(pj,qj)

    # compute m*, s*, ad*
    switch(tolower(measure),
           mix = {d <- mstar(xstack,ystack)},
           scale = {d <- sstar},
           absolute = {d <- adstar(xstack,ystack)})

    # store in results matrix
    result[i,3] = d
  }
  result[,1:2] = t(cn)
  return(result)
}

# mix dissimilarity
mstar <- function(x,y){
  len <- length(x)
  return((1/len)*sum((log(x/y)-(1/len)*sum(log(x/y)))^2))
}

# scale dissimilarity
sstar <- function(x,y){
  len <- length(x)
  return(((1/len)*sum(log(x/y)))^2)
}

# absolute dissimilarity
adstar <- function(x,y){
  return(mstar(x,y)+sstar(x,y))
}
