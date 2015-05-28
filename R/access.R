#' Extract parts of a unitted object
#' 
#' The standard accessors \code{x[...]}, \code{x[[...]]}, and \code{x$name} are
#' all available for unitted objects.
#' 
#' \code{x[...]} extracts elements with their units.
#' 
#' @name unitted_access
#' @aliases [ Extract
#' @rdname unitted_access
#' @export
#' @seealso \code{\link{unitted_assign}} for assignment to parts of objects; 
#'   \code{\linkS4class{unitted}} for definition of the unitted class
#'   
#' @param x The unitted data.frame, vector, etc. to be accessed
#' @param ... Arguments passed to accessors, typically including one or more 
#'   extraction indices
#' @return A new data.frame, vector, etc. with the right units still attached.
"[.unitted" <- function(x, ...) {
  if(isTRUE(is.data.frame(x))) {
    new_units <- NA
    x <- deunitted(x, partial=TRUE)
  } else {
    new_units <- get_unitbundles(x)
    x <- deunitted(x)
  }
  
  vx <- NextMethod("[")
  
  unitted(vx, new_units)
}

#' @details \code{x[[...]]} drops most attributes but retains units.
#' 
#' @aliases [[
#' @rdname unitted_access
#' @export
"[[.unitted" <- function(x, ...) {
  if(isTRUE(is.data.frame(x))) {
    new_units <- NA
    x <- deunitted(x, partial=TRUE)
  } else {
    new_units <- get_unitbundles(x)
    x <- deunitted(x)
  }
  
  vx <- NextMethod("[[")
  
  unitted(vx, new_units)
}

#' @details \code{x$name} extracts a named column, with units, from a unitted data.frame
#'
#' @aliases $
#' @rdname unitted_access
#' @export
#' 
#' @param name The name of the column to be extracted
"$.unitted" <- function(x, name) {
  # NextMethod gives errors about promises being an unacceptable type for 'col',
  # so use [[ instead of NextMethod. According to ?`$`, x$name is equivalent to
  # x[["name", exact = FALSE]]
  x[[name, exact=FALSE]]
}


