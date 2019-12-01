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

#' IndLaspeyres_t
#'
#' @param p1 numeric vector of prices in period 1
#' @param p0 numeric vector of prices in period 0
#' @param q1 numeric vector of quantities in period 1
#' @keywords internal
#' @noRd
indLaspeyres_t <- function(p0, p1, q1){
  q1*(p1 - p0)
}

#' Calculate a price indicator
#'
#' This calculates a price indicator
#'
#' @param x data frame with input data
#' @param pvar character string for the name of the price column
#' @param qvar character string for the name of the quantity column
#' @param pervar character string for the name of the time period variable
#' @param prodID character string for the name of the product ID column
#' @param method character string for the indicator method. Valid options
#' are "laspeyres", "paasche", "bennet", or "montgomery".
priceIndicator <- function(x, pvar, qvar, pervar, prodID, method){

  validMethods <- c("laspeyres", "paasche", "bennet", "montgomery")
  if(!(method %in% validMethods)){
    stop("Invalid method chosen")
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



}
