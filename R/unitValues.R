#' unitValues
#'
#' A function to aggregate price and quantity data to unit values
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @return A dataframe containing columns for product identifier, time period,
#' quantities, and unit values.
#' @export
unitValues <- function(x,pvar,qvar,pervar,prodID){

  # number of periods
  n <- max(x[[pervar]],na.rm = TRUE)

  # loop over all periods ...
  means_it <- lapply(1:n,function(i){
    # subset period i
    xt <- x[x[[pervar]]==i,]
    # loop over all products
    means_i <- lapply(unique(xt[[prodID]]),function(id){
      # subset the period 'i', product 'id' data
      xti <- xt[xt[[prodID]]==id,]
      # calculate expenditure for this item
      exp <- sum(xti[[pvar]]*xti[[qvar]])
      # calculate total quantity
      qit <- sum(xti[[qvar]])
      # calculate unit value
      unitValue <- exp/qit
      # create a vector containing the id, time, total quantity and unit value
      result <- cbind(id,i,qit,unitValue)
      colnames(result) <- c(prodID,"period",qvar,"unitValue")
      return(result)
    })
    # bind results for products in period 'i' into a matrix
    result_i <- do.call(rbind,means_i)
  })
  # bind results for all 'i' into a matrix
  result_it <- do.call(rbind,means_it)
  return(as.data.frame(result_it))
}

#' monthIndex
#'
#' A function to create a month index variable
#'
#' @param x A vector or column of dates
#' @export
monthIndex <- function(x){
  month <- as.numeric(format(x,"%m"))+
    (as.numeric(format(x,"%Y"))-
       as.numeric(format(x[1],"%Y")))*12-
    as.numeric(format(x[1],"%m"))
  return(month)
}

#' quarterIndex
#'
#' A function to create a quarter index variable
#'
#' @param x A vector or column of dates
#' @export
quarterIndex <- function(x){
  quarter <- ceiling(as.numeric(format(x,"%m"))/3)+
    (as.numeric(format(x,"%Y"))-
       as.numeric(format(x[1],"%Y")))*4-2
  quarter <- quarter - (quarter[1]-1)
  return(quarter)
}
