## Representation and display

#### show ####

#' Show a unitted object
#' 
#' Displays a unitted object, including its units and data type whenever possible
#' 
#' @param object the unitted object to display
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
#' @param ... other arguments to \code{\link{print}}
#' @return An invisible unitted vector
#' @export
print.unitted <- function(x,...) {
  .unitted_print(x, ...)
}

# Print a unitted object
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
#' @rdname print.unitted
#' @inheritParams base::print.data.frame
#' @examples
#' head(u(data.frame(x = u(1:500,"A"), y = u(runif(500),"B"), z = u(500:1,"C"))))
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

#' Print a unitted_tbl_df
#' 
#' @rdname print.unitted
#' @param n Number of rows to show, as in \code{\link[tibble]{print.tbl_df}}
#' @param width Width of text output to generate, as in
#'   \code{\link[tibble]{print.tbl_df}}
#' @importFrom dplyr dim_desc trunc_mat
#' @import tibble
#' @examples
#' tibble::as_tibble(u(list(x = u(1:500,"A"), y = u(runif(500),"B"), z = u(500:1,"C"))))
setMethod(
  ".unitted_print", "tbl_df",
  function(x, ..., n = NULL, width = NULL) {
    # comparable to getAnywhere(print.tbl_df)
    y <- trunc_mat(v(x), n = n, width = width)
    # reformat the trunc_mat as unitted
    for(colnm in names(y$table)) {
      y$table[colnm] <- u(unclass(y$table[[colnm]]), get_units(x)[colnm])
    }
    y$table <- u(y$table)
    print(y, n=n, width=width)
    
    invisible(x)
  }
)


#### str ####

#' Return the structure of a unitted object.
#' 
#' Works like str() but displays units-relevant information more elegantly
#' 
#' @name unitted_str
#' @rdname unitted_str
#' @export
#' @importFrom utils str
#' @param object The object whose structure is to be displayed
#' @param ... other arguments passed to \code{str}
str.unitted <- function(object, ...) {
  cat(" ",class(object)," (",paste(get_units(object), collapse=";"),"):", sep="")
  str(S3Part(object, strictS3=TRUE), ...)
  invisible()
}

#' @rdname unitted_str
#' @importFrom utils str
#' @export
str.unitted_data.frame <- function(object, ...) {
  if (!is.data.frame(object)) {
    warning("str.unitted_data.frame() called with non-data.frame -- coercing to one.")
    object <- unitted(data.frame(object))
  }
  cat("unitted data.frame:\t", nrow(object), " obs. of  ", (p <- length(object)), 
      " variable", if (p != 1) 
        "s", if (p > 0) 
          ":", "\n", sep = "")
  args_say_length <- length(l <- list(...)) && any("give.length" == names(l))
  mapply(function(col, colnm) {
    cat(" $ ", colnm, ":", sep="")
    if(args_say_length) 
      str(col, ...)
    else
      str(col, give.length=FALSE, ...)
  }, object, names(object))
  invisible()
}

#' @rdname unitted_str
#' @export
str.unitted_NULL <- function(object, ...) {
  cat(paste0("unitted ", substring(class(object)[1], 9), " (", get_units(object), ")\n"))
  invisible()
}

#### .str ####

#' importFrom utils str
#' @rdname dot.str
#' @param object the object to describe
#' @param ... other args for future development
.str <- function(object, ...) {
  if(isS4(object)) {
    cat("S4 object of class '", class(object), "' (package '", attr(class(object),"package"), "'):\n",sep="")
    if(!(".Data" %in% slotNames(class(object)))) {
      s3p <- S3Part(object, strictS3=TRUE)
      cat("[S3Part]: ")
      str(s3p)
    }
    for(sl in slotNames(class(object))) {
      cat("@ ",sl,": ",sep="")
      str( slot(object, sl) ) 
    }
  } else {
    cat("S3 object of oldClass ",paste0(paste0("'", oldClass(object), "'"),collapse=", "),":\n",sep="")
    str(object)
  }
  
  invisible()
}
