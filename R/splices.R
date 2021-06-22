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



#' function to calculate the new spliced datapoint
#'
#' This function passes the right values to the
#' splice function for the different types of splicing method.
#'
#' @param x the base by which we multiply the splice factor to
#' get the new point. Usually this is the last period of the old
#' window.
#' @param oldWindow the previously computed window (i.e. the last w
#' periods of the index that we are extending).
#' @param newWindow the new window computed in order to extend the index
#' @param method the splicing method to use. Possible options are
#' 'movement', 'window', 'mean', 'half', or a number representing
#' the period to use for the splice.
#' @keywords internal
#' @noRd
splice_t <- function(x, oldWindow, newWindow, method="mean"){

  if(is.numeric(method)){
    pt <- x*splice(method, oldWindow, newWindow)
  }
  else {
    switch(method,
           movement = {pt <- x*splice(length(newWindow)-1, oldWindow, newWindow)},
           window = {pt <- x*splice(1, oldWindow, newWindow)},
           mean = {pt <- x*meanSplice(oldWindow, newWindow)},
           half = {pt <- x*splice((length(newWindow)-1)/2+1, oldWindow, newWindow)}
    )
  }

  return(pt)
}


#' compute the splice factor for any given overlapping period
#'
#' @param period the period to use for the overlap
#' @param oldWindow the old window
#' @param newWindow the new window
#' @keywords internal
#' @noRd
splice <- function(period, oldWindow, newWindow){
  w <- length(newWindow)
  spliceFactor <- (newWindow[w]/newWindow[period])/(oldWindow[w]/oldWindow[period+1])
}


#' mean splicing using the geometric mean of all overlapping periods
#'
#' This is wrapper around \code{splice} to use the geometric mean
#' of all possible combinations of overlap period.
#'
#' @param oldWindow the old window
#' @param newWindow the new window
#' @keywords internal
#' @noRd
meanSplice <- function(oldWindow, newWindow){
  w <- length(newWindow)
  pvector <- matrix(0,nrow=w-1,ncol=1)

  for(l in 1:(w-1)){
    pvector[l,1] <- splice(l, oldWindow, newWindow)
  }
  return(geomean(pvector))
}

