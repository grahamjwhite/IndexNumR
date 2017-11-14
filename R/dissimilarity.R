#' Computes measures of relative dissimilarity between all periods
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
#' @references Diewert, W.E. (2002). "Similarity and Dissimilarity Indexes:
#' An Axiomatic Approach" Discussion Paper No. 0210, Department of Economics,
#' University of British Columbia.
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
  return(as.data.frame(res))
}

#' Computes mix, scale and absolute dissimilarity measures
#'
#' This is a function to compute the Fox, Hill and Diewert 2004
#' dissimilarity measures.
#'
#' @param x A dataframe
#' @param pvar string identifying the price variable in x
#' @param qvar string identifying the quantity variable in x
#' @param pervar string identifying the time period variable in x
#' @param prodID string identifying the product id variable in x
#' @param measure choice of dissimilarity measure. Valid options
#' are mix, scale or absolute.
#' @param combine specifies how to combine the price and quantity vectors.
#' "stack" stacks the price and quantity vectors, "geomean" computes
#' separate dissimilarity measures for prices and quantities then takes
#' the geometric mean of these.
#' @return A matrix where the first two columns are the possible combinations
#' of periods and the third column is the dissimilarity measure.
#' @references Fox, K.J., R.J. Hill and W.E. Diewert (2004),
#' "Identifying outliers in multi-output models", Journal of
#' Productivity Analysis, 22, 7394, 2004.
#' @export
mixScaleDissimilarity <- function(x, pvar, qvar, prodID, pervar,
                                  measure="absolute", combine="geomean"){

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

    # if stacking requested stack p and q into one vector
    if(tolower(combine)=="stack"){
      xvec <- c(pi,qi)
      yvec <- c(pj,qj)
      # compute m*, s*, ad*
      switch(tolower(measure),
             mix = {d <- mstar(xvec,yvec)},
             scale = {d <- sstar(xvec,yvec)},
             absolute = {d <- adstar(xvec,yvec)})
    }
    else if(tolower(combine)=="geomean"){
      switch(tolower(measure),
             mix = {d <- (mstar(pi,pj)*mstar(qi,qj))^0.5},
             scale = {d <- (sstar(pi,pj)*sstar(qi,qj))^0.5},
             absolute = {d <- (adstar(pi,pj)*adstar(qi,qj))^0.5})
    }

    # store in results matrix
    result[i,3] = d
  }
  result[,1:2] = t(cn)
  colnames(result) <- c("period_i","period_j","dissimilarity")
  return(as.data.frame(result))
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

#' Finds periods to link using minimum dissimilarity.
#'
#' Function to compute the maximum similarity chain links from
#' a measure of dissimilarity. The procedure works as described in
#' Diewert and Fox (2017). It first links period 2 to period 1.
#' Then for each period t, from periods 3,...,T it searches
#' among the periods 1,...,t-1 for the period that is most
#' similar (least dissimilar) to period t.
#'
#' @param x a matrix containing a dissimilarity measure where
#' the first two columns are the indices and the third column
#' is the dissimilarity measure.
#' @export
maximiumSimilarityLinks <- function(x){

  # initialise some matrices
  periods <- nrow(x[x$period_i==1,])+1
  # results[,1] = current period index
  # results[,2] = linking period
  # results[,3] = the dissimilarity measure for linked periods
  result <- matrix(1,nrow=periods,ncol=3)
  # period 1 link is 1 and dissimilarity is 0
  result[1,3] = 0
  # period 2 must be linked to period 1
  result[2,1] = 2
  result[2,2] = 1
  result[2,3] = x[x$period_j==2 & x$period_i==1,3]

  # from period 3 onwards, find the minimum dissimilarity linking period
  for(i in 3:periods){
    # get the similarity measures between period i and previous periods
    temp <- x[x$period_j==i & x$period_i<i,]
    # store minimum dissimilarity
    # current period
    result[i,1] = i
    # linking period
    result[i,2] = which.min(temp$dissimilarity)
    # the dissimilarity measure
    result[i,3] = min(temp$dissimilarity)
  }
  colnames(result) <- c("xt","x0","dissimilarity")
  return(as.data.frame(result))
}
