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



#' Dataset of prices and quantities on four products
#'
#' A constructed dataset containing the prices and quantities of
#' four products over a twelve month period, assuming CES preferences.
#'
#' @format A data frame with 48 rows and 4 columns:
#' \describe{
#'   \item{time}{time period}
#'   \item{prices}{constructed prices}
#'   \item{quantities}{constructed quantities}
#'   \item{prodID}{product identifier}
#' }
#' @source Computed using procedure in W.E. Diewert and K.J. Fox (2017),
#' "Substitution Bias in Multilateral Methods for CPI Construction Using
#' Scanner Data", Discussion Paper 17-02, Vancouver School of Economics,
#' The University of British Columbia.
"CES_sigma_2"
