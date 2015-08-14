#' @include 01-parse.R 02-unitbundle.R
NULL

## Unitted object construction

#### Superclass Definition ####

#' A fusion of data and units
#' 
#' A unitted object is a data object - a vector, data.frame, array, etc. - and 
#' its associated units. These units will be propagated through addition, 
#' multiplication, and many other common operations on data. Unitted objects are
#' created by calling \code{\link{unitted}()} or \code{\link{u}()}, and the data
#' can be retrieved by calling \code{\link{deunitted}()} or \code{\link{v}()}.
#' Units can be extracted as character strings with \code{\link{get_units}()}.
#' Most of the time, you'll find that unitted objects can do everything that
#' their un-unitted data could do, but with smart units to make your analyses
#' more readable and less vulnerable to typos or omissions.
#' 
#' Every unitted object contains a .Data 'slot', though this is not formally
#' recognized as such in the documentation.
#' 
#' @name unitted-class
#' @rdname unitted-class
#' @exportClass unitted
#'   
#' @slot units The units. Depending on the .Data type, the units slot may 
#'   contain a single units bundle (e.g., "kg ha^-1"), a matrix or array of 
#'   bundles, or a list of bundles. Any of these may be retrieved with 
#'   \code{\link{get_units}}.
#'   
#' @seealso \code{\link{unitted}} for class and subclass construction, 
#'   \code{\link{deunitted}} for recovering the data from the unitted object, 
#'   and \code{\link{unitted_Ops}} for smart units behaviors.
#'   
#' @examples
#' # unitted object creation
#' miles_vec <- unitted(1:5,"mi")
#' hours_vec <- unitted(7,"hr")
#' 
#' # unitted object manipulation
#' miles_vec / hours_vec
#' miles_vec^2
#' hours_vec + hours_vec
#' hours_vec == hours_vec
setClass("unitted", slots=c(units="ANY"))


#### Generic Constructor ####

#' Attach units to data
#' 
#' A \code{unitted} object is the fusion of data and units. The \code{unitted()}
#' function, or its alias \code{u()}, constructs an object inheriting from the 
#' \code{\linkS4class{unitted}} class.
#' 
#' The short constructor name (\code{u}) makes it easy to write clean code that 
#' enforces units consistency throughout your data analysis. The underlying 
#' constructor function is called \code{unitted()} but aliased to \code{u()}. 
#' Similarly, units can be removed from data using the \code{\link{deunitted}()}
#' function or its alias \code{\link{v}()}.
#' 
#' @name u
#' @rdname unitted
#' @export u
#'   
#' @param object A vector, data.frame, array, matrix, list, or S4 object 
#'   containing data
#' @param units A specification of the units to attach to \code{object}. The 
#'   class and form of \code{units} depends on the class of \code{object}; see 
#'   Details
#' @param ... Further arguments passed to unitbundle() for each unitted vector
#'   or element created
u <- function(object, units=NA, ...) {
  unitted(object, units, ...)
}

#' @name unitted
#' @rdname unitted
#' @exportMethod unitted
setGeneric(
  "unitted", 
  function(object, units=NA, ...) {
    standardGeneric("unitted")
  }
)



#### Subclass Definitions ####

# These definitions must occur after the generic unitted constructor is defined.

#' Define a custom subclass of \code{unitted}
#' 
#' There is already a unitted class defined for each of the basic classes 
#' (numeric, logical, character, ...), many of the most common S3 data 
#' structures (factor, Date, POSIXct), and both lists and data.frames. And for 
#' the most part, only the numeric types will make sense as unitted types. 
#' However, there may still be cases where you need yet another class for yet 
#' another data type. \code{new_unitted_class} helps you define such a class.
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
#' @param superclass.name The character name of the S4 class that will form the 
#'   core of the new unitted class. The new class will have the name 
#'   "unitted_[superclass.name]".
#' @param overwrite Logical. If TRUE, the new setClass call will be executed 
#'   even if the same superclass.name has already been defined during this
#'   session.
#' @examples
#' # only works during package build, at least at the moment
new_unitted_class <- function(superclass.name, overwrite=FALSE) {
  # manage the superclass class definition
  if(superclass.name %in% c('tbl_df')) setOldClass(superclass.name) # special case
  class_def <- getClassDef(superclass.name)
  if(is.null(class_def)) {
    stop(paste0(superclass.name," must be registered with setOldClass or defined with setClass before calling new_unitted_class()"))
  }

  # check whether the proposed class already exists
  new_name <- paste0("unitted_",superclass.name)
  if(new_name %in% names(getClass("unitted")@subclasses)) {
    if(overwrite)
      warning(paste(new_name,"was already a registered unitted class; calling setClass again anyway"))
    else
      stop(paste(new_name,"is already a registered unitted class; set overwrite=TRUE to proceed anyway"))
  }

  # create the class (special case: ordered needs extra info)
  if(superclass.name == 'ordered') {
    setClass(new_name, contains=c("unitted", superclass.name), slots=list(.Data='integer', levels='character', units='unitbundle'),
             validity=function(object) TRUE)
  } else {
    setClass(new_name, contains=c("unitted", superclass.name))
  }
    
  # create the constructor named unitted by specific to this class
  setMethod("unitted", superclass.name, function(object, units) {
    new(new_name, object, units=unitbundle(units))
  })
  
  # return the new class name
  new_name
}

# Class and constructor definitions for common unitted classes
# 
# Don't be fooled! This unassuming code block is the heart of unitted class
# definitions.
sapply(c("character","complex","logical","numeric","raw","NULL",
         "factor","Date","POSIXct","POSIXlt",
         "list","data.frame","tbl_df",
         "array","matrix","ts",
         "expression","name","function"), 
       new_unitted_class)
# setOldClass("difftime")
# new_unitted_class("difftime")



#### Specific Constructors ####

#' @details \subsection{Data.frames}{
#'   
#'   In a call to \code{unitted(object, units, ...)} where \code{object} is a 
#'   non-unitted data.frame, \code{object} may contain columns that are 
#'   non-unitted, unitted, or a mix.
#'   
#'   \code{units} may be one of three possibilities: (1) a character vector of 
#'   unit strings or NAs, one per column of the data.frame, (2) a list of the 
#'   same length as \code{ncol(object)} where each element is a units 
#'   specification of one of the forms permitted by \code{\link{unitbundle}()}, 
#'   or (3) either NA or missing, indicating that units should be inferred from 
#'   current units of the data.frame columns.
#'   
#'   If \code{object} contains some unitted columns, the units of those columns 
#'   will be preserved if and only if the corresponding element of \code{units} 
#'   is NA. To overwrite a previously unitted column's units with empty units, 
#'   specify the new units for that column with "". Any non-unitted column with
#'   a corresponding NA in \code{units} is given units of "".
#'   
#'   Known issue: Attaching units to a data.frame with \code{unitted()} creates 
#'   row names for the data.frame even if they were absent before. This is a 
#'   known issue with no known resolution. In other respects, unitted 
#'   data.frames behave very much like non-unitted data.frames.
#'   
#'   }
#'   
#' @rdname unitted
setMethod(
  "unitted", "data.frame",
  function(object, units, ...) {
    #print("u(data.frame)")
    
    if(length(units) != ncol(object)) {
      fail <- TRUE
      if(length(units) == 1) {
        if(!isS4(units[[1]])) {
          if(isTRUE(is.na(units[[1]]))) {
            units <- rep(NA, ncol(object))
            fail <- FALSE
          } 
        }
      }
      if(fail) stop("Number of units must equal number of data.frame columns")
    }
    
    # Overwrite units when current column units are absent and/or units[col] is not NA
    for(col in 1:ncol(object)) {
      if(isS4(units[[col]])) {
        object[,col] <- unitted(object[,col], units[[col]])
      } else {
        object[,col] <- unitted(object[,col], units[[col]])
      }
    }
    # Known bug: The following line creates row names for data.frames even if 
    # they were absent before. This is a property of S4 data.frames that doesn't
    # make a lot of sense to me, but that I also don't see how to skirt. So for 
    # now, I accept that unitted data.frames may be slightly different from
    # plain S3 data.frames in this respect.
    return(new("unitted_data.frame", object, units=NA))
  }
)

#' tbl_dfs work very similarly to data.frames
#' 
#' @rdname unitted
setMethod(
  "unitted", "tbl_df",
  function(object, units, ...) {
    #print("u(tbl_df)")
    
    if(length(units) != ncol(object)) {
      fail <- TRUE
      if(length(units) == 1) {
        if(!isS4(units[[1]])) {
          if(isTRUE(is.na(units[[1]]))) {
            units <- rep(NA, ncol(object))
            fail <- FALSE
          } 
        }
      }
      if(fail) stop("Number of units must equal number of data.frame columns")
    }
    
    # Overwrite units when current column units are absent and/or units[col] is not NA
    for(col in 1:ncol(object)) {
      object[[col]] <- unitted(object[[col]], units[[col]])
    }
    # Known bug: The following line creates row names for data.frames even if 
    # they were absent before. This is a property of S4 data.frames that doesn't
    # make a lot of sense to me, but that I also don't see how to skirt. So for 
    # now, I accept that unitted data.frames may be slightly different from
    # plain S3 data.frames in this respect.
    return(new("unitted_tbl_df", object, units=NA))
  }
)

#' @details \subsection{Arrays and matrices}{
#' 
#' Arrays and matrices are currently only permitted to have one unit apiece. 
#' This may change in the future; if this is a feature you want implemented, 
#' please create an issue on GitHub with information about how you would use 
#' multiple units in an array or matrix if the option were available.
#' 
#' }
#' 
#' @rdname unitted
setMethod(
  "unitted", "array",
  function(object, units, ...) {
    # This method is currently identical to the simple constructor defined above. 
    # This one serves as a placeholder for my intention to implement multi-unit 
    # arrays and matrices. I am thinking about efficient ways to accomplish this. 
    # The simplest is to store a parallel array that contains unitbundles rather 
    # than numeric values. A step up would be to use factors and/or hash tables to 
    # store the results of arithmetic operations between unique pairs of 
    # unitbundles during matrix or array arithmetic, reducing the total number of 
    # computations. It might also be possible to allow only some of the dimensions
    # of a matrix or array to be unitted, implying that other dimensions have the
    # same unit all along them. But that's an advanced feature for down the road.
    
    #print("u(array)")
    #http://stackoverflow.com/questions/11857658/assignment-of-s4-r-objects-to-a-matrix-why-does-this-work
    new("unitted_array", object, units=unitbundle(units))
  }
)

#' @details \subsection{Lists}{
#'   
#'   Even non-unitted lists may contain unitted elements (or a mixture of
#'   unitted and non-unitted elements). However, there are advantages to
#'   defining the list itself as unitted - specifically, operations such as
#'   print(), show(), or arithmetic will treat the entire list as unitted,
#'   displaying and applying units as appropriate to each element.
#'   
#'   A unitted list, unlike a list of unitted elements, may have exactly one
#'   unitbundle for the entire list.
#'   
#'   }
#'   
#' @rdname unitted
setMethod(
  "unitted", "list",
  function(object, units, ...) {
    #print("u(list)")
    #warning("The implementation of unitted lists is currently primitive - one unit bundle per list.")
    new("unitted_list", object, units=unitbundle(units))
  }
)

#' @details \subsection{Already-unitted objects}{
#'   
#'   The \code{unitted} or \code{u} function may be used to replicate an 
#'   already-unitted object or replace its units with new ones. When
#'   \code{object} is unitted and the \code{units} argument is \code{NA}, the
#'   constructor returns the data untouched. If the \code{units} argument is
#'   anything else, the call to \code{unitted()} replaces the units of
#'   \code{object} with the new units.
#'   
#'   }
#'   
#' @rdname unitted
setMethod(
  "unitted", "unitted",
  function(object, units, ...) {
    #print("u(unitted)")
    if(isTRUE(isS4(units))) {
      callGeneric(deunitted(object), units, ...)
    } else if(!isTRUE(is.na(units))) {
      callGeneric(deunitted(object), units, ...)
    } else {  
      return(object)
    }
  }
)


#### as.data.frame ####

#' Construct a unitted element of a data.frame
#' 
#' \code{data.frames} are constructed by applying \code{as.data.frame()} to each
#' element. Thus, \code{as.data.frame.unitted()} is called whenever an argument 
#' to \code{data.frame()} is unitted. \code{as.data.frame.unitted()} can handle 
#' inputs of any subclass of \code{unitted}. Data.frame elements constructed 
#' with as.data.frame.unitted continue to store their units as members of the 
#' complete data.frame, although those units may not be visible until/unless the
#' data.frame is itself made unitted by a call to \code{u(mydf)}.
#' 
#' @param x A unitted object
#' @param ... Other arguments passed to \code{as.data.frame()}
#' @return A unitted data.frame element
as.data.frame.unitted <- function(x, ...) {
  .unitted_as.data.frame(object=x, ...)
}
setGeneric(".unitted_as.data.frame", function(object, ...) {
  standardGeneric(".unitted_as.data.frame")
})
setMethod(
  ".unitted_as.data.frame", "ANY",
  function(object, ...) {
    # Vectors (including lists unless overridden) are the simple and typical
    # case. Remove the units, route to a new as.data.frame call, and then add
    # units back to the resulting column of the 1-column data.frame (but not to
    # the data.frame itself)
    df <- do.call("as.data.frame",list(list(deunitted(object)), ...))
    df[[1]] <- unitted(df[[1]], get_unitbundles(object))
    names(df) <- NULL
    return(df)
  }
)
setMethod(
  ".unitted_as.data.frame", "array",
  function(object, ...) {
    df <- do.call("as.data.frame",list(list(deunitted(object)), ...))
    for(col in 1:dim(df)[2]) {
      df[[col]] <- unitted(df[[col]], get_unitbundles(object))
    }
    names(df) <- NULL
    return(df)
  }
)
setMethod(
  ".unitted_as.data.frame", "data.frame",
  function(object, ...) {
    return(object)
  }
)
#' Override the default S3 for as.data.frame.tbl_df
#' 
#' Exporting this function seems to make it available for dispatch but still 
#' hidden. From here we could route back to the S4 dispatch system with 
#' ".unitted_as.data.frame(object=x, ...)", but that seems like a waste of
#' processing time since we come here first.
#' 
#' @param x The unitted_tbl_df to convert to a data.frame
#' @param ... Additional arguments passed to as.data.frame
#' @examples
#' x <- as_data_frame(u(data.frame(x=u(1:3,"k"), y=u(3:5, "g"))))
#' as.data.frame(x)
#' @export
as.data.frame.unitted_tbl_df <- function(x, ...) {
  return(u(as.data.frame(v(x), ...), get_unitbundles(x)))
}


#### Deconstructors ####

#' Extract data from a unitted object
#' 
#' Unitted objects consist of data, units, and unitted class information. The 
#' function \code{deunitted()} and its alias \code{v()} remove the units and 
#' class information or, equivalently, extract the data from the unitted object.
#' 
#' @name v
#' @rdname deunitted
#' @export v
#' @seealso \code{\link{u}()} and \code{\link{unitted}()} for the construction 
#'   of unitted objects; \code{\linkS4class{unitted}} for the definition of the 
#'   unitted class
#'   
#' @param object A unitted object
#' @param ... Other arguments passed to \code{deunitted}
#' @return A non-unitted data object
v <- function(object, ...) {
  deunitted(object, ...)
}

#' @name deunitted
#' @rdname deunitted
#' @exportMethod deunitted
setGeneric("deunitted", function(object, ...) {
  standardGeneric("deunitted")
})

#' @rdname deunitted
#'   
#' @details In general, \code{deunitted()} and \code{v()} simply extract the
#' data part of the unitted object.
setMethod(
  "deunitted", "unitted",
  function(object, ...) {
    # And what if the object's class is unitted extending another S4 class?
    return(S3Part(object, strictS3=TRUE))
  }
)

#' @rdname deunitted
setMethod(
  "deunitted", "unitted_factor",
  function(object, ...) {
    if('ordered' %in% object@.S3Class) {
      object@.S3Class <- object@.S3Class[-which(object@.S3Class == 'ordered')]
      fac <- S3Part(object, strictS3=TRUE)
      ordered(fac)
    } else {
      return(S3Part(object, strictS3=TRUE))
    }
  }
)

#' @rdname deunitted
setMethod(
  "deunitted", "unitted_NULL",
  function(object, ...) {
    return(NULL)
  }
)

#' @rdname deunitted
#'   
#' @details With unitted data.frames and lists, you have a choice between 
#'   partial and complete deunitting (specified by the \code{partial} argument).
#'   Complete deunitting removes units and unitted class attributes both from
#'   the container (the data.frame or list) and its elements (the columns or
#'   list elements). Partial deunitting only removes the container units and
#'   unitted class attributes.
#'   
#' @param partial logical. Should the data.frame or list be fully deunitted, such that 
#'   no elements will be left with units, or partially deunitted, such that the 
#'   object that is returned is not itself unitted but may have unitted 
#'   elements?
#' @examples
#' x <- u(data.frame(x = u(1:500,"A"), y = u(runif(500),"B"), z = u(500:1,"C")))
#' v(x)
setMethod(
  "deunitted", "unitted_data.frame",
  function(object, partial=FALSE, ...) {
    if(!partial) {
      object@.Data <- lapply(object@.Data, function(col) { deunitted(col) })
    }
    return(S3Part(object, strictS3=TRUE))
  }
)
#' @rdname deunitted
#' @examples
#' x <- as_data_frame(u(data.frame(x = u(1:500,"A"), y = u(runif(500),"B"), z = u(500:1,"C"))))
#' str(v(x))
#' str(v(x, partial=TRUE))
setMethod(
  "deunitted", "unitted_tbl_df",
  function(object, partial=FALSE, ...) {
    if(!partial) {
      return(as_data_frame(lapply(S3Part(object, strictS3=TRUE), function(col) { deunitted(col) })))
    } else {
      return(S3Part(object, strictS3=TRUE))
    }
  }
)
#' @rdname deunitted
setMethod(
  "deunitted", "unitted_list",
  function(object, partial=FALSE, ...) {
    if(!partial) {
      object@.Data <- lapply(object@.Data, function(col) { deunitted(col) })
    }
    return(S3Part(object, strictS3=TRUE))
  }
)

#' @rdname deunitted
#'   
#' @details Non-unitted data.frames, tbl_dfs, and lists may also be deunitted:
#'   This operation always removes units from the data.frame columns or list 
#'   elements.
setMethod(
  "deunitted", "data.frame",
  function(object, ...) {
    as.data.frame(lapply(object, function(col) { deunitted(col) }))
  }
)
#' @rdname deunitted
setMethod(
  "deunitted", "tbl_df",
  function(object, ...) {
    as_data_frame(lapply(object, function(col) { deunitted(col) }))
  }
)
#' @rdname deunitted
setMethod(
  "deunitted", "list",
  function(object, ...) {
    lapply(object, function(elem) { deunitted(elem) })
  }
)

#' @rdname deunitted
#'   
#' @details If \code{object} is neither a list nor a data.frame and is not
#' unitted, it will be returned intact.
setMethod(
  "deunitted", "ANY",
  function(object, ...) {
    object
  }
)

#### Helper functions ####

# ' The internal, reasonably efficient, unsafe (little error checking) method for
# ' setting an object's units
# ' 
# ' \code{.set_units()} of a non-unitted class creates a new unitted object with 
# ' the specified units.
# ' 
# ' \code{.set_units()} of a unitted vector, array, or matrix class accepts a 
# ' single unitbundle to associate with that object. NA is NOT an acceptable 
# ' value of new.units.
# ' 
# ' \code{.set_units()} of a unitted data.frame accepts a list of unitbundles (or
# ' NAs), one per data.frame column in \code{columns}. NA is NOT an acceptable 
# ' value of new.units. The length of new.units is not checked.
# ' 
# ' @param object The object whose units should be changes
# ' @param new.units The new unit or units in their internal representation 
# '   (currently a unitbundle)
# ' @param columns (data.frames only) The numeric column numbers to which
# '   new.units should be applied.
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
#' get_unitbundles() of a non-unitted class returns NA.
#' 
#' get_unitbundles() of a unitted vector, array, or matrix class returns the single 
#' unitbundle associated with that object.
#' 
#' get_unitbundles() of a data.frame or unitted_data.frame returns a named list of
#' unitbundles (or NAs), one per data.frame column.
#' 
#' @param object The object whose units should be returned
#' @param ... other arguments passed to class-specific methods
#' @return A unitbundle or list of unitbundles, each representing one set of 
#'   units
setGeneric(
  "get_unitbundles", 
  function(object, ...) {
    NA
  }
)
setMethod(
  "get_unitbundles", "unitted",
  function(object, ...) {
    object@units
  }
)
setMethod(
  "get_unitbundles", "data.frame",
  function(object, recursive=TRUE, ...) {
    if(recursive) {
      lapply(object, function(col) { get_unitbundles(col) })
    } else {
      NA
    }
  }
)
setMethod(
  "get_unitbundles", "unitted_data.frame",
  function(object, recursive=TRUE, ...) {
    if(recursive) {
      setNames(unlist(lapply(object@.Data, get_unitbundles)), object@names)
    } else {
      NA
    }
  }
)
setMethod(
  "get_unitbundles", "unitted_tbl_df",
  function(object, recursive=TRUE, ...) {
    if(recursive) {
      setNames(unlist(lapply(S3Part(object, strictS3=TRUE), get_unitbundles)), object@names)
    } else {
      NA
    }
  }
)
setMethod(
  "get_unitbundles", "list",
  function(object, recursive=TRUE, ...) {
    if(recursive) {
      lapply(object, function(col) { get_unitbundles(col) })
    } else {
      NA
    }
  }
)
setMethod(
  "get_unitbundles", "unitted_list",
  function(object, recursive=FALSE, ...) {
    if(recursive) {
      # '@names' is not technically a slot for S4 lists, but I think this is an
      # efficient way to access names anyway.
      setNames(unlist(lapply(object@.Data, get_unitbundles)), object@names)
    } else {
      object@units
    }
  }
)
