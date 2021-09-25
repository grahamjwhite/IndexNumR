#' IndexNumR: A package for computing index numbers
#'
#' @section Author:
#' Graham White
#'
#' @description
#' IndexNumR is a package for computing bilateral and multilateral index numbers.
#' The package has been designed with performance in mind, to enable computing
#' index numbers on large datasets within a reasonable timeframe. It also aims
#' to make a large number of index number methods available, along with access to datasets
#' to enable research and experimentation.
#'
#' @section Notes:
#' I'd like to thank all those that have commented on, or tested the code so that
#' it could be improved. In particular, I'd like to thank Professor Kevin
#' Fox at the University of New South Wales for his support and input.
#'
#' Some function parameters can have a considerable impact on the outputs, so it is
#' recommended that the user read the documentation for these functions carefully.
#'
#' @section Vignettes:
#' There is very detailed information about the functions in the package vignette, which
#' can be accessed with,
#'
#' \code{browseVignettes("IndexNumR")}.
#'
#' @section Bilateral index functions:
#'
#' These functions compute bilateral indexes
#'
#' \itemize{
#'    \item {\code{\link{priceIndex}}}
#'    \item {\code{\link{quantityIndex}}}
#' }
#'
#' @section Multilateral index functions:
#'
#' These functions compute multilateral indexes
#'
#' \itemize{
#'    \item {\code{\link{GEKSIndex}}}
#'    \item {\code{\link{GKIndex}}}
#'    \item {\code{\link{WTPDIndex}}}
#' }
#'
#' @section Data preparation functions:
#'
#' Use these functions to perform various operations on the data before
#' using other functions, such as index number functions.
#'
#' \itemize{
#'     \item{\code{\link{unitValues}}}
#' }
#'
#' @section Data exploration functions:
#'
#' Use these to learn more about the characteristics of your dataset.
#'
#' \itemize{
#'     \item{\code{\link{evaluateMatched}}}
#'     \item{\code{\link{values}}}
#' }
#'
#' @section Sample data:
#'
#' IndexNumR has one sample dataset,
#'
#' \itemize{
#'     \item{\code{\link{CES_sigma_2}}},
#' }
#'
#' and a function for generating small datasets,
#'
#' \itemize{
#'     \item{\code{\link{CESData}}},
#' }
#'
#' and a function for accessing the Dominicks Finer Foods scanner data,
#'
#' \itemize{
#'     \item{\code{\link{dominicksData}}}.
#' }
#'
#' @section Differences approach to index numbers:
#'
#' These functions are referred to as indicators, to distinguish them
#' from the bilateral and multilateral index functions which use the
#' ratio approach.
#'
#' \itemize{
#'     \item{\code{\link{priceIndicator}}}
#'     \item{\code{\link{quantityIndicator}}}
#' }
#'
#' @section Time index functions:
#'
#' Index functions in IndexNumR generally need a time period variable.
#' These functions will compute the required time period variable,
#' depending on the frequency required.
#'
#' \itemize{
#'     \item{\code{\link{weekIndex}}}
#'     \item{\code{\link{monthIndex}}}
#'     \item{\code{\link{quarterIndex}}}
#'     \item{\code{\link{yearIndex}}}
#' }
#'
#' @docType package
#' @name IndexNumR
NULL
#> NULL
