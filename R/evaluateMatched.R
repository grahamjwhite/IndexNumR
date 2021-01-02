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
#' but the matching needs to know which periods are being compared. Default is chained.
#' @return A list of two matrices, one for expenditures and one for counts.
#' The first four columns present the base period information
#' base_index (the base time period), base (base period expenditure or count),
#' base_matched (the expenditure or count of the base period after matching),
#' base_share (share of total expenditure in the base period that remains after
#' matching). Columns 5-8 are defined analogously for the current period. The matched
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
#'
#' The count matrix has two additional columns, "new" and "leaving". The new column
#' gives the number of products that exist in the current period but not the base period.
#' The leaving column gives the count of products that exist in the base period
#' but not the current period. Matching removes both of these types of products.
#' @examples
#' # create CES_sigma_2 dataset removing the observation in time period 4
#' # on product 1
#' df <- CES_sigma_2[!(CES_sigma_2$time==4 & CES_sigma_2$prodID==1),]
#' # evaluate the overlap between periods for this dataset assuming
#' # a chained index
#' evaluateMatched(df, pvar="prices", qvar="quantities", pervar="time",
#' prodID = "prodID", output="chained")
#' @export
evaluateMatched <- function(x,pvar,qvar,pervar,prodID,output="chained"){

  # check that a valid output type is chosen
  validOutput <- c("chained","fixedbase")
  if(!(tolower(output) %in% validOutput)){
    stop("Not a valid output type. Please choose from chained or fixedbase.")
  }

  # initialise some things
  n <- max(x[[pervar]], na.rm=TRUE)
  colheadsExp <- c("base_index","base","base_matched","base_share",
                "current_index","current","current_matched","current_share")
  colheadsCount <- c(colheadsExp, "new", "leaving")
  expenditure <- matrix(NA,n-1,8)
  colnames(expenditure) <- colheadsExp
  counts <- matrix(NA,n-1,10)
  colnames(counts) <- colheadsCount

  # if fixed base requested, set xt0 to the first period data
  if(tolower(output)=="fixedbase"){
    xt0 <- x[x[[pervar]]==1,]
    expenditure[,"base_index"] <- 1
    expenditure[,"base"] <- sum(xt0[[pvar]]*xt0[[qvar]])
    counts[,"base_index"] <- 1
    counts[,"base"] <- length(unique(xt0[[prodID]]))
  }

  for(i in 2:n){

    # calculate expenditure and counts prior to matching
    if(!(tolower(output)=="fixedbase")){
      xt0 <- x[x[[pervar]]==i-1,]
      expenditure[i-1,"base_index"] <- i-1
      expenditure[i-1,"base"] <- sum(xt0[[pvar]]*xt0[[qvar]])
      counts[i-1,"base_index"] <- i-1
      counts[i-1,"base"] <- length(unique(xt0[[prodID]]))
    }
    else {
      xt0 <- x[x[[pervar]]==1,]
    }

    xt1 <- x[x[[pervar]]==i,]

    expenditure[i-1,"current_index"] <- i
    expenditure[i-1,"current"] <- sum(xt1[[pvar]]*xt1[[qvar]])
    counts[i-1,"current_index"] <- i
    counts[i-1,"current"] <- length(unique(xt1[[prodID]]))

    counts[i-1,9] <- length(xt1[!(xt1[[prodID]] %in% xt0[[prodID]])])
    counts[i-1,10] <- length(xt0[!(xt0[[prodID]] %in% xt1[[prodID]])])

    # remove the unmatched items
    xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
    xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]

    # calculate the matched expenditures and counts
    expenditure[i-1,3] <- sum(xt0[[pvar]]*xt0[[qvar]])
    counts[i-1,3] <- length(unique(xt0[[prodID]]))
    expenditure[i-1,7] <- sum(xt1[[pvar]]*xt1[[qvar]])
    counts[i-1,7] <- length(unique(xt1[[prodID]]))
  }

  # compute the shares
  expenditure[,4] <- expenditure[,3]/expenditure[,2]
  expenditure[,8] <- expenditure[,7]/expenditure[,6]
  counts[,4] <- counts[,3]/counts[,2]
  counts[,8] <- counts[,7]/counts[,6]

  return(list(expenditure=expenditure,counts=counts))
}
