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
