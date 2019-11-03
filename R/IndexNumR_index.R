#' default IndexNumR_index object constructor
#'
#' Creates a new IndexNumR_index object. Not exported; this is exposed by IndexNumR_index().
#' @param x an n by 1 matrix containing the price or quantity index
#' @param method the index number method used to compute the
#' index
#' @param sample whether matching was requested
#' @param output whether period-on-period or chained output was chosen
#' @param dates dates to be set as rownames for the IndexNum_R object
#' @param chainMethod the method of chaining chosen.  The default
#' is "" if the chainMethod is not specified
#' @param sigma the value of sigma for a CES index, default is NA
#' @keywords internal
#' @noRd
IndexNumR_index <- function(x, method, sample, output, dates, chainMethod = "",
                      sigma = NA){

  stopifnot(is.matrix(x))

  if(all(is.na(dates))){
    rowDim <- 1:nrow(x)
  }
  else {
    rowDim <- format(dates, "%d-%m-%Y")
  }

  return(structure(.Data = x,
                   class = c('IndexNumR_index'),
                   dimnames = list(rowDim, 'index'),
                   method = method,
                   sample = sample,
                   output = output,
                   chainMethod = chainMethod,
                   sigma = sigma))
}

#' data frame conversion
#'
#' @param x an IndexNumR_index object
#' @param row.names optional character vector of row names
#' @param optional logical. see ?as.data.frame
#' @param ... additional parameters to be passed to functions
#' @method as.data.frame IndexNumR_index
#' @export
#' @noRd
as.data.frame.IndexNumR_index <- function(x, row.names=NULL, optional=FALSE, ...){
  UseMethod("as.data.frame", indexData(x))
}


#' IndexNumR_index print
#'
#' Overrides the print function so that it acts on the index element
#' of the IndexNumR_index object
#'
#' @param x an IndexNumR_index object
#' @param ... other arguments
#' @export
#' @noRd
print.IndexNumR_index <- function(x, ...){
  cat(paste("A", attributes(x)$method, "index of length", nrow(x), "\n"))
  print(indexData(x))
}

#' IndexData
#'
#' Gets the index data as a numeric vector without the IndexNumR attributes
#'
#' @param x an IndexNumR_Index
#' @export
indexData <- function(x){
  UseMethod("indexData")
}

#' IndexData.IndexNumR_Index generic
#'
#' Gets the index data as a numeric vector without the IndexNumR attributes
#'
#' @param x an IndexNumR_index
#' @method indexData IndexNumR_index
#' @export
#' @noRd
indexData.IndexNumR_index <- function(x){

  retVal <- x
  attributes(retVal) <- list(dim = dim(x), dimnames = dimnames(x))

  return(retVal)
}


