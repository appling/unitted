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

#' The exported, client-side method for acquiring an object's units in string
#' representation.
#' 
#' @rdname get_units
#' @return A string representing one set of units
setMethod(
  "get_units", "unitted",
  function(object, ...) {
    get_units(get_unitbundles(object), ...)
  }
)

#' @rdname get_units
#' @importFrom stats setNames
#' @param recursive logical. If \code{object} is a list or data.frame, should
#'   the units attached to each element be returned?
setMethod(
  "get_units", "unitted_list",
  function(object, recursive=FALSE, ...) {
    if(recursive) {
      setNames(unlist(lapply(object@.Data, get_units)),names(object))
    } else {
      get_units(get_unitbundles(object))
    }
  }
)

#' @rdname get_units
#' @importFrom stats setNames
setMethod(
  "get_units", "list",
  function(object, recursive=TRUE, ...) {
    if(recursive) {
      setNames(unlist(lapply(object@.Data, get_units)),names(object))
    } else {
      NA
    }
  }
)

#' @rdname get_units
#' @importFrom stats setNames
setMethod(
  "get_units", "data.frame",
  function(object, recursive=TRUE, ...) {
    if(recursive) {
      setNames(unlist(lapply(object@.Data, get_units)),names(object))
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
#' @param nounits.handler closure. The function to call if x or its elements 
#'   have no units. Good options are stop, warning, message, and function(msg)
#'   {}.
#' @return If units are as expected, returns return.values[[1]] (by default, x).
#'   Otherwise stops on an error (if violation.handler=stop) or returns 
#'   return.values[[2]].
#' @export
verify_units <- function(
  x, expected.units, return.values=list(x,NULL), 
  violation.handler=stop, nounits.handler=warning) {
  
  if(!is.unitted(x)) {
    if(is.list(x)) {
      # no warning for data.frames and lists unless they have no unitted elements
      if(!any(sapply(x, is.unitted))) {
        nounits.handler("First value is not unitted and has no unitted elements")
      }
    } else {
      nounits.handler("First value is not unitted")
    }
  }
  
  # decide how to check units
  check_inner_units <- (is(x, "data.frame") | is(x, "list")) & !is(x, "unitted_list")
  check_outer_units <- (is(x, "vector") & !is(x, "list")) | is(x, "matrix") | is(x, "array") | is(x, "unitted_list")
  
  # set up givenunits and wantunits (unitbundles or lists of unitbundles) and
  # unitsmatch (a T/F vector, T for match, F for mismatch)
  if(check_inner_units) {
    if(is.null(names(expected.units))) {
      if(length(expected.units)!=length(x)) {
        msg <- paste0("Conflicting dimensions for given units (",length(x),") and expected units (",length(expected.units),").")
        violation.handler(msg)
        return(return.values[[2]])
      }
    }
    givenunits <- get_unitbundles(x, recursive=TRUE)
    wantunits <- lapply(expected.units, function(eu) { unitbundle(eu) })
    # Allow for expected.units to define the order and set of names in x that should be checked
    wantnames <- names(wantunits)
    if(!is.null(wantnames)) {
      if(any(table(wantnames)>1)) {
        stop("Names of expected.units must be unique")
      }
      matches <- match(wantnames, names(givenunits))
      if(any(is.na(matches))) {
        stop("If any expected.units are named, all must be named and elements of names(x)")
      }
      givenunits <- givenunits[matches]
    }
    unitsmatch <- mapply(function(g,w) { g == w }, givenunits, wantunits)
    if(!all(unitsmatch)) {
      mismatches <- which(!unitsmatch)
      msg <- paste0(
        "Unexpected units: given '",get_units(givenunits[mismatches]),
        "', expected '",get_units(wantunits[mismatches]),"'",
        ifelse(rep(is.null(dim(x)),length(mismatches)),"",
               paste0(" in column or list element ",(1:length(givenunits))[mismatches])),
        collapse="\n  ")
    }
    
  } else if(check_outer_units) {
    if(length(expected.units)!=1) {
      msg <- paste0("Conflicting dimensions for given units (",1,") and expected units (",length(expected.units),").")
      violation.handler(msg)
      return(return.values[[2]])
    }
    givenunits <- get_unitbundles(x, recursive=FALSE)
    wantunits <- unitbundle(expected.units)
    unitsmatch <- givenunits == wantunits
    if(!unitsmatch) {
      msg <- paste0(
        "Unexpected units: given '",get_units(givenunits),
        "', expected '",get_units(wantunits),"'")
    }
    
  } else { 
    stop("Unrecognized data type in unitted object")
  }
  
  # If one or more units are mismatched, give a violation with the msg composed
  # above. Also/otherwise return the requested value as appropriate.
  if(!all(unitsmatch)) {
    violation.handler(msg) 
    return(return.values[[2]])
  } else {
    return(return.values[[1]])
  }
}
