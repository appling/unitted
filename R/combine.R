#' Combine unitted elements into a unitted vector
#' 
#' Because a unitted vector has the same units for every element, all elements 
#' passed to c.unitted() must have the same units when recursive=FALSE. When
#' recursive=TRUE, elements may include lists or data.frames, but the base
#' elements (vectors, columns, etc.) of those list elements must be unitted.
#' 
#' @inheritParams base::c
#' @return A unitted vector
#' @export
c.unitted <- function(..., recursive=FALSE) {
  listargs <- list(...)
  if(length(listargs) == 0) return(NULL)
  return(recursive_c.unitted(listargs, NULL, recursive))
}

#' Helper to c.unitted
#' 
#' Combines elements of listarg, recursively if recursive==TRUE
#' 
#' @param listarg A list of elements to combine
#' @param recursive Logical. Same as the recursive argument to c()
#' @return A unitted vector
#' @keywords internal
recursive_c.unitted <- function(listarg, newunits, recursive) {
  # Permitted objects for concatenation are (1) atomic elements with the same 
  # units as all the rest, or (2) lists, pairlists, or data.frames containing
  # such elements if recursive==TRUE
  vlist <- lapply(listarg, function(x) {
    if(is.unitted(x)) {
      if(is.atomic(x)) { # the most common case: atomic, unitted element
        # Don't have to do anything yet
      } else if(is.data.frame(x)) {
        x <- recursive_c.unitted(x, newunits, recursive)
      } else {
        stop("All elements must be atomic or lists/pairlists/data.frames")
      }
    } else { # !is.unitted(x)
      if(!recursive) {
        stop("All elements must be unitted unless recursive==TRUE")
      } else if (!is.list(x)) {
        stop("When recursive==TRUE, elements may include lists/pairlists/data.frames, but base elements must all be unitted")
      } else { # recursive & is.list(x) == TRUE
        x <- recursive_c.unitted(x, newunits, recursive)
      }
    }
    if(is.null(newunits)) {
      newunits <<- get_units(x) # Grab units the first time they're available
    } else {
      if(get_units(x) != newunits) stop(paste("All elements must have the same units;",get_units(x),"!=",newunits))
    }
    vx <- v(x)
    return(vx)
  })
  vc <- do.call("c",c(vlist,list(recursive=recursive)))
  if(is.null(newunits)) stop("Could not find units for c.unitted")
  return(u(vc,newunits)) 
}
