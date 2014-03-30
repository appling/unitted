## Unitted object construction

#### Superclass Definition ####

#' Defines a generic unitted object that could contain a vector, matrix, array, 
#' or data.frame with units attached
#' 
#' A unitted object is the original data object with units attached. These units
#' will be propagated through addition, multiplication, and other operations you
#' perform on the unitted object.
#' 
#' The data part of a unitted object is S3 if the original data object was S3, 
#' and S4 if the original object was S4. However, methods dispatch can be S3 or 
#' S4.
#' 
#' A short function name (u) is essential for using this package to write clean 
#' code that enforces units consistency throughout data analysis operations. The
#' underlying constructor function is called unitted(), which may be useful if 
#' u() is overwritten by another package, but this will seldom be the case.
#' 
#' @slot .Data The data to which units are attached
#' @slot units The units. Depending on the .Data type, the units slot may 
#'   contain a single units bundle (e.g., "kg ha^-1"), a matrix or array of 
#'   bundles, or a list of bundles. Any of these may be retrieved with 
#'   \code{\link{get_units}}.
#' @seealso \code{\link{unitted}} for class and subclass construction,
#'   \code{\link{deunitted}} for recovering the data from the unitted object,
#'   and \code{\link{arithmetic}} for smart units behaviors.
#' @family classes
#' @examples
#' uvec <- u(1:5,"ants-on-a-log")
#' uvec * uvec
setClass("unitted", slots=c(units="ANY"))

#### Generic Constructor ####

#' Attach units to data
#' 
#' If data is already unitted and the following argument is NA, this just 
#' returns data. If the following argument is anything else, this replaces the 
#' units completely.
#' 
#' @param data A vector, array, or data.frame containing the data
#' @param unit Character, data.frame, or list specifying one or more sets of
#'   units (depending on the data type of \code{object})
setGeneric(
  "unitted", 
  function(object, units=NA, ...) {
    standardGeneric("unitted")
  }
)
u <- unitted

#### Subclass Definitions and Constructors ####

#' Define a custom class with units tracking
#' 
#' There is already a unitted class to each of the basic classes, many of the 
#' most common S3 data structures (factor, Date, POSIXct), and both lists and 
#' data.frames. And for the most part, only the numeric types will make sense as
#' unitted types. However, there may still be cases where you need still another
#' class for still another data type.
#' 
#' Your new class will have to be S4; I have found no way to circumvent this 
#' requirement. However, a close S4 counterpart can be constructed for nearly 
#' any S3 class. See the examples.
#' 
#' Your new class should also have a .Data part to it, because only those 
#' computations that apply to the .Data part will affect and be affected by the 
#' units. The default behavior of unitted subclasses expects a single unitbundle
#' in the units slot, which means a single set of units per object. Data.frames 
#' and lists are the notable exceptions.
#' 
#' It is also important to distinguish between S3/S4 classes and S3/S4 methods. 
#' Even though you must use an S4 class, you may use any combination of S3 and 
#' S4 methods; all of these will be correctly dispatched on objects inheriting 
#' from the unitted class, providing that you define your classes correctly and 
#' that the dispatch behavior you want is within the abilities of the particular
#' dispatch system (S3 or s4).
#' 
#' @param superclass.name The character name of the S4 class to be extended by 
#'   the new unitted class. The new class will have the name, 
#'   "unitted_[superclass.name]".
#' @param overwrite Logical. If TRUE, the new setClass call will be executed 
#'   even if the same superclass.name has been
#' @examples
#' # An S4 class to be made unitted
#' setClass("myS4flower", contains="numeric", slots=c(leaves="numeric", petals="numeric", color="character"))
#' new_unitted_class("myS4flower") 
#' petunia <- new("unitted_myS4flower", c(frontyard=7, backyard=3, planters=12), leaves=4, petals=5, color="purple", units="stems ft^-2")
#' petunia * u(1/2,"living")
#' 
#' # An S3 class to be made unitted
#' two_cities <- c("It was the best of times, it was the worst of times, ",
#'                 "it was the age of wisdom, it was the age of foolishness, ",
#'                 "it was the epoch of belief, it was the epoch of incredulity, ",
#'                 "it was the season of Light, it was the season of Darkness, ",
#'                 "it was the spring of hope, it was the winter of despair...")
#' class(two_cities) <- "quote"
#' attr(two_cities, "author") <- "Charles Dickens"
#' print.quote <- function(x) { cat(paste(paste(x, collapse="\n"),"\n   - ",attr(x,"author"))) }
#' print(two_cities)
#' # Now make it unittable
#' setClass("quote", contains="character", slots=c(author="character"))
#' # And now make it unitted
#' new_unitted_class("quote", overwrite=TRUE)
#' two_cities_unitted <- new("unitted_quote", two_cities, author=attr(two_cities, "author"), units="lines")
#' see_spot_run_unitted <- new("unitted_quote", c("Here is Spot.","See Spot run.","Run, Spot, Run!"), author="unknown", units="sentences")
#' print(two_cities_unitted)
#' print(see_spot_run_unitted)
#' sum(!is.na(see_spot_run_unitted))
new_unitted_class <- function(superclass.name, overwrite=FALSE) {
  class_def <- getClassDef(superclass.name)
  if(is.null(class_def)) {
    stop(paste0(superclass.name," must be registered with setOldClass or defined with setClass before calling new_unitted_class()"))
  }
  new_name <- paste0("unitted_",superclass.name)
  if(new_name %in% names(getClass("unitted")@subclasses)) {
    if(overwrite)
      warning(paste(new_name,"was already a registered unitted class; calling setClass again anyway"))
    else
      stop(paste(new_name,"is already a registered unitted class; set overwrite=TRUE to proceed anyway"))
  }
  setClass(new_name, contains=c("unitted", superclass.name))
  setMethod("unitted", superclass.name, function(object, units) {
    new(new_name, object, units=unitbundle(units))
  })
  new_name
}


# Class and constructor definitions for common unitted classes
# 
# Don't be fooled! This unassuming code block is the heart of unittted class
# definitions.
sapply(c("character","complex","logical","numeric","raw","NULL",    #"integer",
         "factor","ordered","Date","POSIXct","POSIXlt",
         "list","data.frame",
         "array","matrix","ts",
         "expression","name","function"), 
       new_unitted_class)


#' Replace units of a unitted object
#' 
#' A currently unitted object may be reunitted with this function.
#' 
#' @param object The currently unitted object
#' @param units The new units to be applied. If the units argument is NA, the 
#'   current object will be returned untouched. If the units argument is 
#'   anything else, the units of the current object will be replaced with these 
#'   new units. Empty units of "" are distinct from non-units (NA), with empty
#'   units usually implying known units with the same numerator and denominator,
#'   e.g., g/g or points/points.
setMethod(
  "unitted", "unitted",
  function(object, units, ...) {
    #print("u(unitted)")
    if(isTRUE(is.na(units))) {
      return(object)
    } else {
      callGeneric(deunitted(object), units, ...)
    }
  }
)

#' Create a unitted list
#' 
#' Even non-unitted lists may contain unitted elements (or a mixture of unitted 
#' and non-unitted elements). However, there are advantages to defining the list
#' itself as unitted - specifically, operations such as print(), show(), or
#' arithmetic will treat the entire list as unitted, displaying and applying
#' units as appropriate to each element.
setMethod(
  "unitted", "list",
  function(object, units, ...) {
    #print("u(list)")
    warning("The implementation of unitted lists is currently primitive - one unit bundle per list.")
    new("unitted_list", object, units=unitbundle(units))
  }
)

#' Create a unitted array
#' 
#' This method is currently identical to the simple constructor defined above. 
#' This one serves as a placeholder for my intention to implement multi-unit 
#' arrays and matrices. I am thinking about efficient ways to accomplish this. 
#' The simplest is to store a parallel array that contains unitbundles rather 
#' than numeric values. A step up would be to use factors and/or hash tables to 
#' store the results of arithmetic operations between unique pairs of 
#' unitbundles during matrix or array arithmetic, reducing the total number of 
#' computations. It might also be possible to allow only some of the dimensions
#' of a matrix or array to be unitted, implying that other dimensions have the
#' same unit all along them. But that's an advanced feature for down the road.
setMethod(
  "unitted", "array",
  function(object, units) {
    #print("u(array)")
    #http://stackoverflow.com/questions/11857658/assignment-of-s4-r-objects-to-a-matrix-why-does-this-work
    new("unitted_array", object, units=unitbundle(units))
  }
)

#' Create a unitted data.frame
#' 
#' If the object data.frame contains some unitted columns, these units will be 
#' preserved, but only if the corresponding element of unitstrs is NA. To 
#' overwrite a previously unitted column's units with empty units, specify the 
#' units for that column with "" rather than NA.
#' 
#' @param object A non-unitted data.frame containing columns that are non-unitted,
#'   unitted, or a mix.
#' @param units One of three possibilities: (1) a character vector of unit 
#'   strings, one per column of the data.frame, (2) a list where each element is
#'   a units specification of one of the forms permitted by unitbundle(), or (3)
#'   NA, indicating that units should be inferred from current units of the 
#'   data.frame columns (a non-unitted column is assumed to have units "")
#' @return A fully unitted data.frame having units specified by units (or, 
#'   when units is NA or all of its elements are NA, by the original units
#'   of the individual columns)
#' @family constructors
setMethod(
  "unitted", "data.frame",
  function(object, units) {
    #print("u(data.frame)")
    
    if(length(units) == 1) {
      if(isTRUE(is.na(units[[1]]))) {
        #units <- rep(NA, ncol(object))
        return(new("unitted_data.frame", object, units=NA))
      }
    } else if(length(units) != ncol(object)) {
      stop("Number of units must equal number of data.frame columns")
    }
    
    # Overwrite units when current column units are absent and/or units[col] is not NA
    for(col in 1:ncol(object)) {
      if(!is.na(units[[col]])) {
        object[,col] <- unitted(object[,col], units[[col]])
      }
    }
    return(new("unitted_data.frame", object, units=NA))
  }
)

#' Construct a unitted element of a (not-necessarily-united) data.frame from an
#' already unitted object
#' 
#' This is also the function that will be applied to calls of 
#' data.frame(unitted_x, y, ...) where unitted_x (and possibly y, etc.) is a
#' unitted object; if called on a non-unitted object, the units will be assumed
#' to be "".
#' 
#' @param x A unitted object
#' @param ... Other arguments passed to as.data.frame()
as.data.frame.unitted <- function(x, ...) {
  .unitted_as.data.frame(x, ...)
}
setGeneric(".unitted_as.data.frame", function(object, ...) {
  standardGeneric(".unitted_as.data.frame")
})
setMethod(
  ".unitted_as.data.frame", "vector",
  function(object, ...) {
    # Vectors (including lists unless overridden) are the simple and typical
    # case. Remove the units, route to a new as.data.frame call, and then add
    # units back to the resulting column of the 1-column data.frame (but not to
    # the data.frame itself)
    df <- do.call("as.data.frame",list(list(deunitted(object)), ...))
    df[[1]] <- unitted(df[[1]], .get_units(object))
    names(df) <- NULL
    return(df)
  }
)
setMethod(
  ".unitted_as.data.frame", "array",
  function(object, ...) {
    df <- do.call("as.data.frame",list(list(deunitted(object)), ...))
    for(col in 1:dim(df)[2]) {
      df[[col]] <- unitted(df[[col]], .get_units(object))
    }
    names(df) <- NULL
    return(df)
  }
)
setMethod(
  ".unitted_as.data.frame", "data.frame",
  function(object, ...) {
    #names(object) <- NULL
    return(object)
  }
)

#### Deconstructors ####

#' Removes units and all "unitted" class attributes from a unitted object
#' 
#' @aliases deunitted, v
#' @param y A unitted object. A non-unitted object will be returned intact.
#' @export
#' @family constructors
setGeneric("deunitted", function(object, ...) {
  standardGeneric("deunitted")
})
v <- deunitted
setMethod(
  "deunitted", "ANY",
  function(object, ...) {
    object
  }
)
setMethod(
  "deunitted", "unitted",
  function(object, ...) {
    return(S3Part(object, strictS3=TRUE))
  }
)
setMethod(
  "deunitted", "unitted_NULL",
  function(object, ...) {
    return(NULL)
  }
)
setMethod(
  "deunitted", "unitted_data.frame",
  function(object, partial=FALSE, ...) {
    if(!partial) {
      object@.Data <- lapply(object@.Data, function(col) { deunitted(col) })
    }
    return(S3Part(object, strictS3=TRUE))
  }
)


#### Helper functions ####

#' The internal, reasonably efficient, unsafe (little error checking) method for
#' setting an object's units
#' 
#' \code{.set_units()} of a non-unitted class creates a new unitted object with 
#' the specified units.
#' 
#' \code{.set_units()} of a unitted vector, array, or matrix class accepts a 
#' single unitbundle to associate with that object. NA is NOT an acceptable 
#' value of new.units.
#' 
#' \code{.set_units()} of a unitted data.frame accepts a list of unitbundles (or
#' NAs), one per data.frame column in \code{columns}. NA is NOT an acceptable 
#' value of new.units. The length of new.units is not checked.
#' 
#' @param object The object whose units should be changes
#' @param new.units The new unit or units in their internal representation 
#'   (currently a unitbundle)
#' @param columns (data.frames only) The numeric column numbers to which
#'   new.units should be applied.
setGeneric(
  ".set_units", 
  function(object, new.units, ...) {
    unitted(object, new.units)
  }
)
setMethod(
  ".set_units", "unitted",
  function(object, new.units) {
    object@units <- new.units
    object
  }
)
setMethod(
  ".set_units", "unitted_data.frame",
  function(object, new.units, columns) {
    for(col in columns) {
      if(!is.na(new.units[[col]])) {
        object@.Data[,col] <- .set_units(object@.Data[,col], new.units[[col]])
      }
    }
    object
  }
)


#' The internal, reasonably efficient method for acquiring an object's units in 
#' the internal representation
#' 
#' .get_units() of a non-unitted class returns NA.
#' 
#' .get_units() of a unitted vector, array, or matrix class returns the single 
#' unitbundle associated with that object.
#' 
#' .get_units() of a unitted data.frame returns a named list of unitbundles (or
#' NAs), one per data.frame column.
#' 
#' @param object The object whose units should be returned
#' @return A unitbundle or list of unitbundles, each representing one set of 
#'   units
setGeneric(
  ".get_units", 
  function(object, ...) {
    NA
  }
)
setMethod(
  ".get_units", "unitted",
  function(object) {
    object@units
  }
)
setMethod(
  ".get_units", "unitted_data.frame",
  function(object) {
    lapply(object, function(col) { .get_units(col) })
  }
)
