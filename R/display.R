## Representation and display

#### show ####

setMethod(
  "show", "unitted",
  function(object) {
    .unitted_print(object)
  }
)
setMethod(
  "show", "unitted_NULL",
  function(object) {
    cat(paste0("unitted ", substring(class(object)[1], 9), " (", get_units(object), ")\n"))
  }
)

#### print ####

#' Print a unitted object
#' 
#' For many data types, \code{print.unitted} includes the units in its output. 
#' For data.frames, for example, it adds a second header row to display the
#' units of each column.
#' 
#' @param x The unitted object
#' @inheritParams base::print
#' @param value The value or values to insert in the specified cell[s]
#' @return An invisible unitted vector
#' @export
print.unitted <- function(x,...) {
  .unitted_print(x, ...)
}

#' Print a unitted object
setGeneric(
  ".unitted_print", 
  function(x, ...) {
    standardGeneric(".unitted_print")
  }
)
setMethod(
  ".unitted_print", "ANY",
  function(x, ...) {
    cat(paste0("unitted ", substring(class(x)[1], 9), " (", get_units(x), ")\n"))
    print(deunitted(x))
  }
)

setMethod(
  ".unitted_print", "NULL",
  function(x, ...) {
    cat(paste0("unitted NULL (use show() or str() to see units)\n"))
  }
)

#' Print a unitted data.frame
#' 
#' @param x The unitted object
#' @inheritParams base::print.data.frame
setMethod(
  ".unitted_print", "data.frame",
  function(x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE) {
    n <- length(row.names(x))
    if (length(x) == 0L) {
      cat(gettextf("data frame with 0 columns and %d rows\n", n))
    }
    else if (n == 0L) {
      fdf <- deunitted(x)
      fdf["U",] <- get_units(x)
      m <- as.matrix(fdf)
      print(m, ..., quote = quote, right = right)
      cat(gettext("<0 rows> (or 0-length row.names)\n"))
    }
    else {
      fdf <- format.data.frame(deunitted(x), digits = digits, na.encode = FALSE)
      fdf <- rbind(U=get_units(x),fdf)
      m <- as.matrix(fdf)
      if (!isTRUE(row.names)) {
        dimnames(m)[[1L]] <- if (identical(row.names, FALSE)) {
          rep.int("", n)
        } else {
          row.names
        }
      }
      print(m, ..., quote = quote, right = right)
    }
  }
)



edit.unitted <- function(name, factor.mode=c("character","numeric"), edit.row.names=any(row.names(name) != 1:nrow(name)), ...) {
  warning("Editing unitted objects is largely uncharted territory. Be alert and tell me if you find bugs.")
  edited <- edit(v(name))
  if(is.data.frame(name)) {
    if((dim(edited) != dim(v(name))) | !identical(rownames(df),rownames(df)) | !identical(colnames(df),colnames(df)))
      stop("Editing has changed the dimensions and/or names of your unitted object. I don't know how to handle this.")
  } else {
    warning("Edited a non-data.frame; did absolutely no error checking for units.")
  }
  edited
}
