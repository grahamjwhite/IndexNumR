#' Evaluate product overlap between periods
#'
#' Evaluate the counts and expenditure for each period with and without
#' matching items across periods.
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param output A character string specifying whether the matching should be
#' done assuming a chained index or a fixed base index. No index is actually computed,
#' but the matching needs to know which periods are being compared.
#' @return A list of two matrices, one for expenditures and one for counts. Each
#' matrix has eight columns. The first four columns present the base period information
#' base_index (the index of the base period), base (base period expenditure or count),
#' base_matched (the expenditure or count of the base period after matching),
#' base_share (share of total expenditure in the base period that remains after
#' matching). Columns 5-8 are defined analagously for the current period. The matched
#' numbers for the base period should be interpreted as the count or expenditure
#' that remains after removal of products that exist in the base period, but not
#' in the current period. That is, products that existed in the base period but no
#' longer exist in the current period are removed by the matching. If new products
#' exist in the current period that were not available in the base period, this
#' does not affect the matched base period expenditure or count. The appearance
#' of new products is captured in the current period matched expenditure and counts.
#' Therefore, a base period share that is less than 1 indicates that products have
#' disappeared, while a current period share less than 1 indicates that new products
#' have appeared.
#' @export
evaluateMatched <- function(x,pvar,qvar,pervar,prodID,output="pop"){

  # initialise some things
  n <- max(x[[pervar]], na.rm=TRUE)
  colheads <- c("base_index","base","base_matched","base_share",
                "current_index","current","current_matched","current_share")
  expenditure <- matrix(0,n,8)
  colnames(expenditure) <- colheads
  counts <- matrix(0,n,8)
  colnames(counts) <- colheads

  # if fixed base requested, set xt0 to the first period data
  if(tolower(output)=="fixedbase"){
    xt0 <- x[x[[pervar]]==1,]
    expenditure[,1] <- 1
    expenditure[,2] <- sum(xt0[[pvar]]*xt0[[qvar]])
    counts[,1] <- 1
    counts[,2] <- length(unique(xt0[[prodID]]))
  }

  for(i in 2:n){

    # calculate expenditure and counts prior to matching
    if(!(tolower(output)=="fixedbase")){
      xt0 <- x[x[[pervar]]==i-1,]
      expenditure[i,1] <- i-1
      expenditure[i,2] <- sum(xt0[[pvar]]*xt0[[qvar]])
      counts[i,1] <- i-1
      counts[i,2] <- length(unique(xt0[[prodID]]))
    }

    xt1 <- x[x[[pervar]]==i,]

    expenditure[i,5] <- i
    expenditure[i,6] <- sum(xt1[[pvar]]*xt1[[qvar]])
    counts[i,5] <- i
    counts[i,6] <- length(unique(xt1[[prodID]]))

    # remove the unmatched items
    xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
    xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]

    # calculate the matched expenditures and counts
    expenditure[i,3] <- sum(xt0[[pvar]]*xt0[[qvar]])
    counts[i,3] <- length(unique(xt0[[prodID]]))
    expenditure[i,7] <- sum(xt1[[pvar]]*xt1[[qvar]])
    counts[i,7] <- length(unique(xt1[[prodID]]))
  }
  # compute the shares
  expenditure[,4] <- expenditure[,3]/expenditure[,2]
  expenditure[,8] <- expenditure[,7]/expenditure[,6]
  counts[,4] <- counts[,3]/counts[,2]
  counts[,8] <- counts[,7]/counts[,6]

  return(list(expenditure=expenditure,counts=counts))
}
