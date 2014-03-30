#' Extract parts of a unitted data.frame, vector, etc.
#' 
#' This function does pretty well for data.frames and vectors but does not 
#' references df[1] like you'd think; rather than returning the first column, 
#' df[1] returns the first row. Be warned.
#' 
#' Bug: if you try to reduce the number of columns in a unitted data.frame, 
#' e.g., chemdat[,-which(names(chemdat)=="removethisone")], then you get an 
#' error. This should be possible.
#' 
#' Bug: if you try to access rows or columns of a unitted data.frame via a list 
#' of T/F values, checkIndex complains.
#' 
#' Bug: ghgchem[,"UNHID",drop=FALSE] is not possible (unused argument (drop =
#' FALSE))
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
  `.unitted_[`(x, ...)
}
setGeneric(
  ".unitted_[", 
  function(x, ...) {
    dots <- as.list(match.call(expand.dots=TRUE)[-c(1L,2L)])
    vx <- do.call(`[`, c(list(deunitted(x)), dots))
    return(u(vx,.get_units(x)))
    
#     print(dots)
#     goodargs <- sapply(seq_along(dots), function(dotnum) { 
#       tryCatch(i <- dots[[dotnum]], error=function(e) {i <<- e})
#       print(i)
#       !is(i,"error") & !is.symbol(i)
#     })
#     has_. <- (length(dots) > 0)
#     vx <- 
#       if(has_.) {
#         do.call(`[`, c(list(deunitted(x)), dots))
#       } else {
#         deunitted(x)[]
#       }
#     unitted(vx, .get_units(x))
  }
)
setMethod(
  ".unitted_[", "data.frame",
  function(x, i, j, ..., drop) {
    dots <- as.list(match.call(expand.dots=FALSE)[-c(1L,2L)])
    has_i <- (!is.null(dots[["i"]]) & !is.symbol(dots[["i"]]))
    has_j <- (!is.null(dots[["j"]]) & !is.symbol(dots[["j"]]))
    has_d <- (!is.null(dots[["d", exact=FALSE]]) & !is.symbol(dots[["d", exact=FALSE]]))
    has_. <- (!is.null(dots[["..."]]) & !is.symbol(dots[["..."]][[1]]))
    if(has_i) force(i)
    if(has_j) force(j)
    if(has_d) force(drop)
    if(has_.) force(...)
    # This is all just routing to pass the right set of arguments to `[.data.frame`:
    vx <-
      if(has_i) {
        if(has_j) {
          if(has_d) {
            if(has_.) {
              deunitted(x, partial=TRUE)[i, j, ..., drop=drop]
            } else {
              deunitted(x, partial=TRUE)[i, j, drop=drop]
            }
          } else {
            if(has_.) {
              deunitted(x, partial=TRUE)[i, j, ...]
            } else {
              deunitted(x, partial=TRUE)[i, j]
            }
          }
        } else {
          if(has_d) {
            if(has_.) {
              deunitted(x, partial=TRUE)[i,  , ..., drop=drop]
            } else {
              deunitted(x, partial=TRUE)[i,  , drop=drop]
            }
          } else {
            if(has_.) {
              deunitted(x, partial=TRUE)[i,  , ...]
            } else {
              deunitted(x, partial=TRUE)[i,  ]
            }
          }
        }
      } else {
        if(has_j) {
          if(has_d) {
            if(has_.) {
              deunitted(x, partial=TRUE)[ , j, ..., drop=drop]
            } else {
              deunitted(x, partial=TRUE)[ , j, drop=drop]
            }
          } else {
            if(has_.) {
              deunitted(x, partial=TRUE)[ , j, ...]
            } else {
              deunitted(x, partial=TRUE)[ , j]
            }
          } 
        } else {
          if(has_d) {
            if(has_.) {
              deunitted(x, partial=TRUE)[ ,  , ..., drop=drop]
            } else {
              deunitted(x, partial=TRUE)[ ,  , drop=drop]
            }
          } else {
            if(has_.) {
              deunitted(x, partial=TRUE)[ ,  , ...]
            } else {
              deunitted(x, partial=TRUE)[ ,  ]
            }
          }
        }
      }
    unitted(vx, NA) 
  }
)

#' Extract parts of an object, dropping attributes (including units, by default)
#' 
#' @param drop.units Logical. If TRUE, drops units, else retains them.
"[[.unitted" <- function(x, ..., drop.units=TRUE) {
  `.unitted_[[`(x, ..., drop.units=drop.units)
}
setGeneric(
  ".unitted_[[", 
  function(x, ..., drop.units) { 
    if(drop.units) {
      deunitted(x)[[...]]
    } else {
      unitted(deunitted(x)[[...]], .get_units(x))
    }
    
  }
)
setMethod(
  ".unitted_[[", "data.frame",
  function(x, ..., drop.units) {
    if(drop.units) {
      deunitted(x, partial=FALSE)[[...]]
    } else {
      unitted(deunitted(x, partial=TRUE)[[...]], NA)
    }
  }
)

#' Extract a column of a unitted data.frame
#' 
#' Returns a vector with the right units still attached.
#' 
#' @param x The data.frame
#' @param col The name of the column to be extracted
#' @return A vector with the right units still attached.
#' @export
"$.unitted" <- function(x, col) {
  failure <- tryCatch({
    # ?`$` says x$name is equivalent to x[[name, exact = FALSE]]
    return(.remove_unitted_class(x)[[col, exact = FALSE]]) 
  }, error = function(e) e )
  stop(failure) # using tryCatch enables traceback on error
}



