#' default IndexNumR_index object constructor
#'
#' Creates a new IndexNumR_index object. Not exported; this is exposed by IndexNumR_index().
#' @param x an n by 1 matrix containing the price or quantity index
#' @param method the index number method used to compute the
#' index
#' @param sample whether matching was requested
#' @param output whether period-on-period or chained output was chosen
#' @param chainMethod the method of chaining chosen.  The default
#' is "" if the chainMethod is not specified
#' @param sigma the value of sigma for a CES index, default is NA
#' @keywords internal
new_IndexNumR_index <- function(x, method, sample, output, chainMethod = "",
                      sigma = NA){
  stopifnot(is.matrix(x))

  return(structure(list(index=x, method=method, sample=sample,
                        output=output, chainMethod=chainMethod,
                        sigma=sigma), class=c("IndexNumR_index")))
}

#' validate an IndexNumR_index object
#'
#' @param x an IndexNumR_index
#' @keywords internal
validate_IndexNumR_index <- function(x){
  # do some checks here...
  x
}

#' Create a new object of class 'IndexNumR_index'
#'
#' @param x an n by 1 matrix containing the price or quantity index
#' @param method the index number method used to compute the
#' index
#' @param sample whether matching was requested
#' @param output whether period-on-period or chained output was chosen
#' @param chainMethod the method of chaining chosen. The default
#' is "" if the chainMethod is not specified
#' @param sigma the value of sigma for a CES index, default is NA
#' @export
IndexNumR_index <- function(x, method, sample, output, chainMethod = "",
                  sigma = NA){
  new_IndexNumR_index(x, method, sample, output, chainMethod, sigma)
}

#' data frame conversion
#'
#' @param x an IndexNumR_index object
#' @param row.names optional character vector of row names
#' @param optional logical. see ?as.data.frame
#' @param ... additional parameters to be passed to functions
#' @method as.data.frame IndexNumR_index
#' @export
as.data.frame.IndexNumR_index <- function(x, row.names=NULL, optional=FALSE, ...){
  UseMethod("as.data.frame", x$index)
}

#' IndexNumR_index subsetting
#'
#' Overrides the subset behaviour so that it acts on the index element
#' of the IndexNumR_index object
#' @param x an IndexNumR_index object
#' @param i row number
#' @param j column number
#' @param ... other parameters
#' @export
`[.IndexNumR_index` <- function(x, i, j, ...){
  x = x$index
  NextMethod()
}

#' IndexNumR_index subset assign
#'
#' Overrides the subset assign behaviour so that it acts on the index element
#' of the IndexNumR_index object
#'
#' @param x an IndexNumR_index object
#' @param i row number
#' @param j column number
#' @param ... other parameters
#' @param value value to be assigned
#' @export
`[<-.IndexNumR_index` <- function(x, i, j, ..., value){
  x = x$index
  NextMethod()
}

#' IndexNumR_index dimension
#'
#' Overrides the dim function so that it acts on the index element
#' of the IndexNumR_index object
#'
#' @param x an IndexNumR_index object
#' @export
dim.IndexNumR_index <- function(x){
  x = x$index
  NextMethod()
}

#' IndexNumR_index transpose
#'
#' Overrides the transpose function so that it acts on the index element
#' of the IndexNumR_index object
#'
#' @param x an IndexNumR_index object
#' @export
t.IndexNumR_index <- function(x){
  x = x$index
  NextMethod()
}

#' IndexNumR_index dimnames
#'
#' Overrides the dimnames function so that it acts on the index element
#' of the IndexNumR_index object
#'
#' @param x an IndexNumR_index object
#' @export
dimnames.IndexNumR_index <- function(x){
  x = x$index
  NextMethod()
}

#' IndexNumR_index dimnames assignment
#'
#' Overrides the dimnames function to assign names to the index element
#' of the IndexNumR_index class
#'
#' @param x an IndexNumR_index object
#' @param value value to assign
#' @export
`dimnames<-.IndexNumR_index` <- function(x, value){
  x = x$index
  NextMethod()
}

#' IndexNumR_index print
#'
#' Overrides the print function so that it acts on the index element
#' of the IndexNumR_index object
#'
#' @param x an IndexNumR_index object
#' @param ... other arguments
#' @export
print.IndexNumR_index <- function(x, ...){
  print(x$index)
}

#' IndexNumR_index Math
#'
#' Overrides the math functions so that they act on the index element
#' of the IndexNumR_index object. For affected functions see ?groupGeneric.
#'
#' @param x an IndexNumR_index object
#' @param ... other arguments
#' @export
Math.IndexNumR_index <- function(x, ...){
  get(.Generic)(x$index)
}

#' IndexNumR_index Ops
#'
#' Overrides the Ops functions so that they act on the index element
#' of the IndexNumR_index object. For affected functions see ?groupGeneric.
#'
#' @param e1 an IndexNumR_index object
#' @param e2 an IndexNumR_index object
#' @export
Ops.IndexNumR_index <- function(e1,e2){
  get(.Generic)(e1$index,e2$index)
}


#' IndexNumR_index Summary functions
#'
#' Overrides the Summary group generic functions so that they act on
#' the index element of the IndexNumR_index object. For affected functions see ?groupGeneric.
#'
#' @param ... further arguments
#' @param na.rm logical: should missing values be removed? Default = FALSE
#' @export
Summary.IndexNumR_index <- function(x, ..., na.rm = FALSE){
  get(.Generic)(x$index, ..., na.rm = FALSE)
}
