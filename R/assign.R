#### $<- unitted_data.frame ####

setMethod(
  "$<-",c(x="unitted_data.frame", value="ANY"), 
  function(x, name, value) {
    if(is(value, "unitted")) {
      v2 <- callNextMethod(x, name, deunitted(value))[[name, exact=FALSE]]
      callNextMethod(x, name, unitted(v2, .get_units(value)))
    } else {
      xcol <- x[[name, exact = FALSE]]
      if(is(xcol,"unitted_NULL")) {
        xu <- NA
      } else {
        xu <- .get_units(xcol)
      }
      v2 <- callNextMethod()[[name, exact=FALSE]]
      callNextMethod(x, name, unitted(v2, xu))
    }
  }
)

#### [<- unitted ####

setMethod(
  "[<-", c(x="unitted", i="ANY", j="missing"), 
  function(x, i, j, ..., value) {
    # print("unitted[i] <- value")
    if(.get_units(x) != .get_units(value)) { # unitbundle != interprets NA and unitbundle() as equal
      stop("Mismatched units in subset replacement: ", get_units(x)," != ", get_units(value))
    }
    dx <- deunitted(x)
    dx[i] <- deunitted(value)
    unitted(dx, .get_units(x))
  }
)

#### [<- unitted_data.frame ####

# ' @param replace.old Logical. Should existing columns whose elements are being replaced also have their units replaced?
.udf_get_replacement_units <- function(x, newx, cols, value, replace.old=FALSE) {
  if(ncol(newx) == ncol(x)) {
    if(replace.old) {
      replace(.get_units(x), cols, .get_units(value))
    } else {
      .get_units(x)
    }
  } else {
    # the data.frame was expanded during the 'replacement', so the new units
    # might include units from value
    if(replace.old) {
      new_units <- rep(NA,ncol(newx)) # a list of empties
      new_units <- replace(new_units, match(names(x), names(newx)), .get_units(x))
      replace(new_units, cols, .get_units(value))
    } else {
      if(is(value, "unitted")) {
        uv <- unname(.get_units(value))
        if(!is.list(uv)) uv <- list(uv)
        c(.get_units(x), uv[which(!(cols %in% seq_len(ncol(x))))])
      } else {
        c(.get_units(x), rep_len(NA,ncol(newx)-ncol(x)))
      }
    }
  }
}

.udf_check_replacement_units <- function(xdest, value) {
  # Extract the units of the destination cells and the units of the replacement
  # value.
  ux <- unname(.get_units(xdest))
  if(!is.list(ux)) ux <- list(ux)
  uv <- unname(.get_units(value))
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

#### [old] ####

# #' Replace cell[s] of a unitted data.frame
# #' 
# #' Bug: If you want to add columns to a unitted data.frame, you have to do it
# #' one at a time. This won't work: 
# #' chemdat[ghgrows,c(ghgcols,"CO2aq_G_APA","CH4aq_G_APA","N2Oaq_G_APA")] <- ghgchem 
# #' where "CO2aq_G_APA","CH4aq_G_APA","N2Oaq_G_APA" are new columns to
# #' chemdat and where ghgchem and chemdat are both unitted already.
# #' 
# #' @param x The data.frame
# #' @param i The row (or column, if j is missing)
# #' @param j The column
# #' @param value The value or values to insert in the specified cell[s]
# #' @return A new data.frame with the specified cells replaced with \code{value}
# #' @export
# "[<-.unitted" <- function(x,i,j,...,value) {
#   call_args <- as.list(match.call())
#   access_args <- call_args[-c(1, 2, length(call_args))]
#   
#   replacee <- do.call(`[`, c(list(x), access_args))
#   assigned <- NextMethod()
#   #as_replaced <- do.call(`[`, c(list(assigned), access_args))
#   
#   msg <- function(replacee, replacement) {
#     paste0("Units mismatch in partial replacement. ",
#            "Replacee units: '",get_units(replacee),
#            "'; replacement units: '",get_units(replacement),"'")
#   }
#   if(is.atomic(.remove_unitted_class(x))) {
#     if(is.null(dim(x))) {
#       # Vectors
#       if(.get_units(replacee) != .get_units(value)) {
#         stop(msg(replacee, value))
#       }
#     } else {
#       # Matrices and arrays
#       if(.get_units(replacee) != .get_units(value)) {
#         stop(msg(replacee, value))
#       }
#     }
#     
#   } else if(is.data.frame(x)) {
#     
#   } else {
#     stop("Unrecognized data type of x in [<-.unitted")
#   }
#   
#   return(assigned)
# 
# }
# 
# 
# #' Replace a column of a unitted data.frame
# #' 
# #' Returns a vector with the right units still attached.
# #' 
# #' @param x The data.frame
# #' @param i The row (or column, if j is missing)
# #' @param j The column
# #' @param value The value or values to insert in the specified cell[s]
# #' @return A unitted vector
# #' @export
# "$<-.unitted" <- function(x,col,value) {
#   if(!is.data.frame(x))
#     stop("The only unitted data type currently supported for $<- replacement\n",
#          "is a data.frame, which this is not.")
#   get("[<-.unitted")(x,,col,value=value)
#   #eval(call("[<-.unitted",x,NULL,col,value))
#   #x[,col] <- value
# }
#         
# 
# #' Replace values within a unitted object
# #' 
# #' @param x The original unitted data object
# #' @param i The row (or column, if j is missing)
# #' @param j The column
# #' @param value The value or values to insert in the specified cell[s]
# #' @return A new unitted data object
# #' @export
# "[[<-.unitted" <- function(x,i,j,...,value) {
#   stop("not yet implemented")
# }
