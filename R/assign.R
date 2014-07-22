#### [<- unitted ####

#' Replace parts of a unitted vector, array, or data.frame
#' 
#' @name unitted_assign
#' @aliases [<- Extract
#' @rdname unitted_assign
#' @export
#' @seealso \code{\link{unitted_access}} for assignment to parts of objects; 
#'   \code{\linkS4class{unitted}} for definition of the unitted class
#'   
#' @param x The unitted data.frame, vector, etc. to be partially replaced
#' @param i (optional) The first replacement index
#' @param j (optional) The second replacement index
#' @param ... Other arguments passed to replacement functions
#' @param value The value(s) with which to replace the indexed elements of x
#' @return \code{x} with its indexed elements replaced by \code{value}
setMethod(
  "[<-", c(x="unitted", i="ANY", j="missing"), 
  function(x, i, j, ..., value) {
    # print("unitted[i] <- value")
    if(get_unitbundles(x) != get_unitbundles(value)) { # unitbundle != interprets NA and unitbundle() as equal
      stop("Mismatched units in subset replacement: ", get_units(x)," != ", get_units(value))
    }
    dx <- deunitted(x)
    dx[i] <- deunitted(value)
    unitted(dx, get_unitbundles(x))
  }
)

#### $<- unitted_data.frame ####

#' @aliases $<-
#' @rdname unitted_assign
#' @export
#' 
#' @param name The name of the column to be replaced
setMethod(
  "$<-",c(x="unitted_data.frame", value="ANY"), 
  function(x, name, value) {
    if(is(value, "unitted")) {
      v2 <- callNextMethod(x, name, deunitted(value))[[name, exact=FALSE]]
      callNextMethod(x, name, unitted(v2, get_unitbundles(value)))
    } else {
      xcol <- x[[name, exact = FALSE]]
      if(is(xcol,"unitted_NULL")) {
        xu <- NA
      } else {
        xu <- get_unitbundles(xcol)
      }
      v2 <- callNextMethod()[[name, exact=FALSE]]
      callNextMethod(x, name, unitted(v2, xu))
    }
  }
)

#### [<- unitted_data.frame ####

# ' @param replace.old Logical. Should existing columns whose elements are being replaced also have their units replaced?
.udf_get_replacement_units <- function(x, newx, cols, value, replace.old=FALSE) {
  if(ncol(newx) == ncol(x)) {
    if(replace.old) {
      replace(get_unitbundles(x), cols, get_unitbundles(value))
    } else {
      get_unitbundles(x)
    }
  } else {
    # the data.frame was expanded during the 'replacement', so the new units
    # might include units from value
    if(replace.old) {
      new_units <- rep(NA,ncol(newx)) # a list of empties
      new_units <- replace(new_units, match(names(x), names(newx)), get_unitbundles(x))
      replace(new_units, cols, get_unitbundles(value))
    } else {
      if(is(value, "unitted")) {
        uv <- unname(get_unitbundles(value))
        if(!is.list(uv)) uv <- list(uv)
        c(get_unitbundles(x), uv[which(!(cols %in% seq_len(ncol(x))))])
      } else {
        c(get_unitbundles(x), rep_len(NA,ncol(newx)-ncol(x)))
      }
    }
  }
}

.udf_check_replacement_units <- function(xdest, value) {
  # Extract the units of the destination cells and the units of the replacement
  # value.
  ux <- unname(get_unitbundles(xdest))
  if(!is.list(ux)) ux <- list(ux)
  uv <- unname(get_unitbundles(value))
  if(!is.list(uv)) uv <- list(uv)
  
  # Compare the units of the data.frame destination cells to those of the
  # replacement value. Throw errors if appropriate; otherwise, return quietly.
  if(length(ux) != length(uv)) {
    stop("Mismatched units lengths in subset replacement: ", length(ux), " != ", length(uv))
  } else if(!all(sapply(seq_len(length(ux)), function(i) { ux[[i]] == uv[[i]] }))) {
    badones <- which(!sapply(seq_len(length(ux)), function(i) { ux[[i]] == uv[[i]] }))
    stop("Mismatched units in subset replacement: \n    ", 
         paste0("Column ",badones," of replacement: destination units (",get_units(ux[badones]),
                ") != replacement units (", get_units(uv[badones]),")", collapse="\n    "))
  }
}

#' @rdname unitted_assign
#' @export
setMethod(
  "[<-", c(x="unitted_data.frame", i="ANY", j="ANY"), 
  function(x, i, j, ..., value) {
    #print("unitted_data.frame[i, j] <- value")
    
    # Do the assignment on non-unitted versions of x and value. This method
    # causes a copying of x. That's too bad, but how else can we reuse the
    # regular data.frame code for replacement?
    newx <- deunitted(x)
    newx[i,j] <- deunitted(value)
    fullcolumns <- all(rownames(x) %in% rownames(x[i,1,drop=FALSE])) #does a data.frame always have to have at least one column? I hope so.
    newx <- u(newx, .udf_get_replacement_units(x, newx, j, value, isTRUE(fullcolumns)))
    .udf_check_replacement_units(newx[i,j], value)
    newx
  }
)

#' @rdname unitted_assign
#' @export
setMethod(
  "[<-", c(x="unitted_data.frame", i="ANY", j="missing"), 
  function(x, i, j, ..., value) {
    # determine which type of call we're dealing with, [i] or [i,]
    argtype <- switch(
      as.character(nargs()),
      "3"="[i]",
      "4"="[i,]",
      stop("Unexpected number of arguments to [<- for unitted_data.frame")
    )
    #print(paste0("unitted_data.frame",argtype," <- value"))
    
    newx <- deunitted(x)
    if(argtype=="[i]") {
      newx[i] <- deunitted(value)
      newx <- u(newx, .udf_get_replacement_units(x, newx, i, value, TRUE))
      .udf_check_replacement_units(newx[i], value)
    } else { # argtype=="[i,]
      newx[i,] <- deunitted(value)
      fullcolumns <- all(rownames(x) %in% rownames(x[i,1,drop=FALSE])) #does a data.frame always have to have at least one column? I hope so.
      newx <- u(newx, .udf_get_replacement_units(x, newx, seq_len(ncol(x)), value, isTRUE(fullcolumns)))
      .udf_check_replacement_units(newx[i,], value)
    }
    newx
  }
)

#' @rdname unitted_assign
#' @export
setMethod(
  "[<-", c(x="unitted_data.frame", i="missing", j="ANY"), 
  function(x, i, j, ..., value) {
    #print("unitted_data.frame[,j] <- value")
    newx <- deunitted(x)
    newx[,j] <- deunitted(value)
    newx <- u(newx, .udf_get_replacement_units(x, newx, j, value, TRUE))
    .udf_check_replacement_units(newx[,j], value)
    newx
  }
)

#' @rdname unitted_assign
#' @export
setMethod(
  "[<-", c(x="unitted_data.frame", i="missing", j="missing"), 
  function(x, i, j, ..., value) {
    #print("unitted_data.frame[, ] <- value")
    newx <- deunitted(x)
    newx[] <- deunitted(value)
    newx <- u(newx, .udf_get_replacement_units(x, newx, seq_len(ncol(x)), value, TRUE))
    .udf_check_replacement_units(newx[], value)
    newx
  }
)


