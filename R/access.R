#' Extract parts of a unitted data.frame, vector, etc.
#' 
#' @param x The data.frame, vector, etc.
#' @param i
#' @param j
#' @param ...
#' @param drop Should the column names be dropped if only a single column 
#'   remains?
#' @return A new data.frame, vector, etc. with the right units still attached.
#' @export
"[.unitted" <- function(x, ...) {
  if(isTRUE(is.data.frame(x))) {
    new_units <- NA
    x <- deunitted(x, partial=TRUE)
  } else {
    new_units <- .get_units(x)
    x <- deunitted(x)
  }
  
  vx <- NextMethod("[")
  
  unitted(vx, new_units)
}

#' Extract parts of an object, dropping most attributes but retaining units
#' 
#' To remove units, use v() before or after calling "[[".
"[[.unitted" <- function(x, ...) {
  if(isTRUE(is.data.frame(x))) {
    new_units <- NA
    x <- deunitted(x, partial=TRUE)
  } else {
    new_units <- .get_units(x)
    x <- deunitted(x)
  }
  
  vx <- NextMethod("[[")
  
  unitted(vx, new_units)
}

#' Extract a column of a unitted data.frame
#' 
#' Returns a vector with the right units still attached.
#' 
#' @param x The data.frame
#' @param col The name of the column to be extracted
#' @return A vector with the right units still attached.
#' @export
"$.unitted" <- function(x, col) {
  # NextMethod gives errors about promises being an unacceptable type for 'col',
  # so use [[ instead of NextMethod. According to ?`$`, x$name is equivalent to
  # x[["name", exact = FALSE]]
  x[[col, exact=FALSE]]
}


