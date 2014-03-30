## Getting properties of unitted objects

#' Determine whether an object is unitted
#' 
#' @param x The potentially unitted object
#' @return Logical. TRUE if x is unitted, FALSE otherwise
#' @export
is.unitted <- function(x) {
  return(is(x,"unitted"))
}


#### get_units ####

#' The exported, client-side method for acquiring an object's units in string representation.
#' 
#' @param y The object whose units should be returned
#' @return A string representing one set of units
setMethod(
  "get_units", "unitted",
  function(object, ...) {
    get_units(.get_units(object))
  }
)
setMethod(
  "get_units", "unitted_data.frame",
  function(object, ...) {
    setNames(unlist(lapply(object@.Data, get_units)),names(object))
  }
)
setMethod(
  "get_units", "unitted_list",
  function(object, recursive=FALSE, ...) {
    if(recursive) {
      unlist(lapply(object@.Data, get_units))
    } else {
      NA
    }
  }
)

#### verify_units ####

#' Assert that a unitted object has the expected units
#' 
#' Throws an error (or calls a user-specified function) if the units are not as 
#' expected. If the first element of return.values is x, verify_units(x) returns
#' x when units are as expected, and the function may therefore be chained with 
#' other commands.
#' 
#' @param x character. The object to test for whether it has the expected units.
#' @param expected.units character. The expected units as a character [vector].
#' @param return.values list(2). The values to return if units are (1) as 
#'   expected, or (2) not as expected. Good options are list(x,NULL) and 
#'   list(TRUE, FALSE). Elements beyond position 2 are ignored.
#' @param violation.handler closure. The function to call if the expectation is 
#'   violated. Good options are stop, warning, message, and function(msg) {}.
#' @return If units are as expected, returns return.values[[1]] (by default, x).
#'   Otherwise stops on an error (if violation.handler=stop) or returns 
#'   return.values[[2]].
#' @export
verify_units <- function(x, expected.units, return.values=list(x,NULL), violation.handler=stop) {
  if(!is.unitted(x)) {
    stop("First value must be unitted")
  }
  givenunits <- unname(get_units(x))
  if(is.null(dim(x)) | is.array(x)) {
    if(length(expected.units)!=1) {
      msg <- paste0("Conflicting dimensions for given units (",1,") and expected units (",length(expected.units),").")
      violation.handler(msg)
      return(return.values[[2]])
    }
    wantunits <- unname(get_units(u(1,expected.units)))
  } else if (is.data.frame(x)) {
    if(length(expected.units)!=ncol(x)) {
      msg <- paste0("Conflicting dimensions for given units (",ncol(x),") and expected units (",length(expected.units),").")
      violation.handler(msg)
      return(return.values[[2]])
    }
    wantunits <- unname(get_units(u(do.call(data.frame,c(1:length(expected.units)+0.0,list())),expected.units)))
  } else { 
    stop("Unrecognized data type in unitted object")
  }
  if(!all(givenunits == wantunits)) {
    mismatches <- which(!(givenunits == wantunits))
    msg <- paste0("Unexpected units: given '",wantunits[mismatches],"', expected '",givenunits[mismatches],"'",
                  ifelse(rep(is.null(dim(x)),length(mismatches)),"",paste0(" in column ",(1:length(givenunits))[mismatches])),
                  collapse="\n  ")
    violation.handler(msg) 
    return(return.values[[2]])
  } else {
    return(return.values[[1]])
  }
}
