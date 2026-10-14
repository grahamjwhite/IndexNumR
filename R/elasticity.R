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
#' to calculate the elasticity. Acceptable options are ces, fisher
#' or satovartia. The ces option equates the 'base period' lloyd-moulton
#' index with the 'current period' lloyd-moulton index.
#' @param lower lower limit to search for sigma.
#' @param upper upper limit to search for sigma.
#' @return A list with three elements: sigma (the average elasticity
#' over all time periods); allsigma (a T-1 by 1 matrix of the estimated
#' elasticities for each time period, except period one); and diff
#' (the value of the difference between the two indexes, check this is zero
#' for all time periods).
#' @examples
#' elasticity(CES_sigma_2,pvar="prices",qvar="quantities",pervar="time",
#' prodID = "prodID")
#' @export
elasticity <- function(x, pvar, qvar, pervar, prodID,
                          compIndex = "ces", lower = -20, upper = 20){

  if(!(tolower(compIndex) %in% c("ces", "fisher", "satovartia"))){
    stop("Not a valid compIndex. Please choose from ces, fisher or satovartia.")
  }

  # check the inputs and sort by time period and product, so that the
  # price and quantity vectors of the two periods are aligned
  x <- prepareData(x, pvar, qvar, pervar, prodID)

  # find the rows for each time period, so that the data for a period can be
  # extracted without scanning the whole dataset
  rows <- periodRows(x, pervar)
  cols <- setdiff(c(pvar, qvar, prodID), "")

  # initialise some things
  n <- max(x[[pervar]],na.rm = TRUE)
  result <- list()
  sigmas <- matrix(0, nrow=n-1, ncol=1)
  diff <- matrix(0,nrow=n-1,ncol=1)
  failed <- integer()

  for(i in 2:n){
    # if pop requested then set xt0 to the previous period
    xt0 <- getPeriod(x, rows, i-1, cols)

    # set xt1 to current period data
    xt1 <- getPeriod(x, rows, i, cols)

    # remove unmatched items
    xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
    xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]

    # set p and q
    p0 <- xt0[[pvar]]
    p1 <- xt1[[pvar]]
    q0 <- xt0[[qvar]]
    q1 <- xt1[[qvar]]

    # find the solution to (CES index - compIndex) = 0
    root <- tryCatch(stats::uniroot(function(x) indexDiff(x,p0,p1,q0,q1,compIndex=compIndex),
                                    lower = lower, upper = upper, tol = 0.00001),
                     error = function(e) NULL)

    # save result
    if(is.null(root)){
      sigmas[i-1] <- NA
      diff[i-1] <- NA
      failed <- c(failed, i)
    } else {
      sigmas[i-1] <- root$root
      diff[i-1] <- root$f.root
    }
  }

  if(length(failed) > 0){
    warning("No solution for sigma was found between lower and upper for periods: ",
            paste(failed, collapse = ", "), ". These were set to NA and excluded from the mean.")
  }

  result$sigma <- mean(sigmas, na.rm = TRUE)
  result$allsigma <- sigmas
  result$diff <- diff

  return(result)
}


#' Computes the difference between a CES index and a comparison index
#'
#' A function, with a root at zero, that computes the difference between
#' the CES index and a comparison index.
#' @keywords internal
#' @noRd
indexDiff <- function(sigma,p0,p1,q0,q1,compIndex="ces"){
  lloyd0 <- lloydMoulton_t0(p0,p1,q0,sigma)
  switch (tolower(compIndex),
          ces = {diff <- lloyd0 - lloydMoulton_tc(p0,p1,q1,sigma)},
          fisher = {diff <- lloyd0 - fisher_t(p0,p1,q0,q1)},
          satovartia = {diff <- lloyd0 - satoVartia_t(p0,p1,q0,q1)}
  )
  return(diff)
}
