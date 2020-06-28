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


#' Generate data assuming CES preferences
#'
#' This function is useful for generating datasets that can be used for
#' testing where the 'true' price index is known. The data are constructed
#' using assumed prices and total expenditure in each period. Expenditure
#' shares and quantities are then computed assuming CES preferences. For
#' further details, see the references.
#'
#' @param sigma the elasticity of substitution parameter
#' @return a dataframe containing time period, prices, quantities and
#' product identifier.
#' @references W.E. Diewert and K.J. Fox (2017),
#' "Substitution Bias in Multilateral Methods for CPI Construction Using
#' Scanner Data", Discussion Paper 17-02, Vancouver School of Economics,
#' The University of British Columbia.
#' @export
#' @examples
#' \dontrun{
#' # generate data assuming the elasticity of substitution is 2
#' CESData(2)
#' }
CESData <- function(sigma){

  # alpha, preference parameteres
  a <- c(0.2, 0.2, 0.2, 0.4)

  # artificial prices
  p <- as.matrix(data.frame(c(2, 1.75, 1.6, 1.5, 1.45, 1.4, 1.35, 1.3, 1.25, 1.2, 1.15, 1.1),
                            c(1, 0.5, 1.05, 1.1, 1.12, 1.15, 1.18, 0.6, 1.2, 1.25, 1.28, 1.3),
                            c(1, 0.95, 0.9, 0.85, 0.4, 0.8, 0.75, 0.72, 0.7, 0.4, 0.7, 0.65),
                            c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.7, 0.65, 0.7, 0.75, 0.75, 0.8)))

  # artificial expenditures
  e <- c(10, 13, 11, 12, 15, 13, 14, 17, 15, 18, 16, 17)
  # matrix for shares
  s <- matrix(0, nrow = 12, ncol = 4)
  # matrix for quantities
  q <- matrix(0, nrow = 12, ncol = 4)

  for(i in 1:12){
    for(j in 1:4){

      s[i,j] <- (a[j]*(p[i,j]^(1-sigma)))/(sum(a*(p[i,]^(1-sigma))))
      q[i,j] <- (e[i]*s[i,j])/p[i,j]

    }
  }

  # pivot to long format
  pLong <- numeric(48)
  qLong <- numeric(48)

  for(i in 1:4){
    pLong[(i*12-11):(i*12)] <- p[,i]
    qLong[(i*12-11):(i*12)] <- q[,i]
  }

  retVal <- data.frame(time = rep(1:12, 4),
                       prices = pLong,
                       quantities = qLong,
                       prodID = c(rep(1, 12), rep(2, 12), rep(3, 12), rep(4, 12)))



  return(retVal)

}
