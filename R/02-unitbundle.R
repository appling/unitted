# Package a set of units (e.g., the numerator and denominator units for a single
# vector) into a single S4 class that can be smart about reading in units
# strings, doing mathematical operations, etc.

#### Class Definition ####

unitbundle <- setClass(
  "unitbundle",
  representation(unitdf="data.frame"),
  prototype(unitdf=data.frame(Unit=character(), Power=numeric(), stringsAsFactors=FALSE)),
  validity=function(object) { 
    valid <- TRUE
    errs <- character()
    if(!isTRUE(all.equal(names(object@unitdf), c("Unit","Power")))) {
      valid <- FALSE
      errs <- c(errs, "unitdf should contain exactly the columns Unit and Power")
    } else if(!isTRUE(is.character(object@unitdf$Unit))) {
      valid <- FALSE
      errs <- c(errs, "unitdf$Unit should be of type 'character'")
    } else if(!isTRUE(is.numeric(object@unitdf$Power))) {
      valid <- FALSE
      errs <- c(errs, "unitdf$Power should be numeric")
    } else if(!isTRUE(all.equal(sort_units(simplify_units(list(object@unitdf)))[[1]], object@unitdf))) {
      valid <- FALSE
      errs <- c(errs, "unitdf should always be sorted")
    }
    return(if(valid) TRUE else errs)
  }
)

#### Constructors ####

#' Methods to create a unitbundle
#' 
#' There are several ways to create a unit bundle. The first argument determines
#' which is used. If the first argument is unlabeled or labeled 'units', then 
#' the class of that argument determines whether it is interpreted as a unitdf 
#' (if it's a data.frame), a unitstr (if it's a character), or a list containing
#' [[1]]=unitvec and [[2]]=powervec. Alternatively, you can directly specify one
#' of those options by labeling your argument[s] as unitdf, unitstr, or unitvec 
#' and powervec.
#' 
#' @param .Object The S4 object; omitted from the argument list in calls
#' @param units A data.frame (unitdf method), character (parse string method), 
#'   list of a character vector and a numeric vector (2-vector method), or 
#'   unitbundle (replica method). Although the code is cleaner, it is slightly 
#'   less efficient to use this units argument rather than explicitly specifying
#'   unitdf, unitstr, or unitvec and powervec. It might be slightly more 
#'   efficient than specifying unitbdl, but almost trivially so.
#' @param ... Other arguments passed to initialize
#' @param unitdf A data.frame (unitdf method) with columns Unit=character, 
#'   Power=numeric. The unit column should not be a factor.
#' @param unitstr A single character string containing one or more units to be 
#'   bundled, e.g. "kg ha^-1 yr^-1"
#' @param unitbld A unitbundle. If this is supplied, the new unitbundle is
#'   simply a replica of unitbdl.
setMethod(
  "initialize", "unitbundle",
  function(.Object, units, ..., unitdf, unitstr, unitbdl) {
    argchoice <- c("units","unitdf","unitstr","unitbdl")[which(!c(missing(units), missing(unitdf), missing(unitstr), missing(unitbdl)))]
    if(length(argchoice) != 1) {
      if(length(argchoice) < 1) {
        # warning("missing units specification; assuming ''")
        # unitbundle() returns empty units for internal convenience; u() will still require a units specification
        return(callNextMethod(.Object, unitdf=data.frame(Unit=character(), Power=numeric(), stringsAsFactors=FALSE)))
      } else if(length(argchoice) > 1) {
        stop("too many units specifications; need exactly one of units, unitdf, unitstr, or unitbdl")
      }
    }
    
    switch(
      argchoice,
      units = {
        msg <- "invalid class for units specification - must be data.frame, character, NA, unitbundle, or missing"
        switch(
          class(units),
          data.frame = callGeneric(.Object, unitdf=units, ...),
          character = callGeneric(.Object, unitstr=units, ...),
          logical = if(isTRUE(is.na(units))) callNextMethod(.Object, unitdf=data.frame(Unit=character(), Power=numeric(), stringsAsFactors=FALSE)) else stop(msg),
          unitbundle = units,
          stop(msg)
        )
      },
      unitdf = {
        if(!isTRUE(all.equal(names(unitdf), c("Unit","Power")))) {
          stop("unitdf must have columns Unit and Power, in that order")
        }
        if(is.factor(unitdf$Unit)) { 
          unitdf$Unit <- levels(unitdf$Unit)[unitdf$Unit]
        }
        if(!is.character(unitdf$Unit)) {
          stop("unitdf$Unit must be character")
        }
        if(all(is.na(unitdf$Power))) {
          unitdf$Power <- as.numeric(unitdf$Power)
        }
        if(!is.numeric(unitdf$Power)) {
          stop("unitdf$Power must be numeric")
        }
        callNextMethod(.Object, unitdf=sort_units(simplify_units(list(unitdf)))[[1]], ...) 
      },
      unitstr = {
        if(length(unitstr) != 1) {
          stop("a character argument describing a units bundle must have length 1")
        }
        callNextMethod(.Object, unitdf=sort_units(simplify_units(parse_units(unitstr)))[[1]], ...) 
      },
      unitbdl = {
        unitbdl
      }
    )
  }
)

#### Display ####

setMethod(
  "show", "unitbundle", 
  function(object){
    cat("unitbundle:",get_units(object))
  }
)


#### Inspect ####

#' Not exported, but presents an abstraction to be used by unitted object 
#' methods.
#' 
#' Think of unitbundles as objects to be created in several forms and retrieved
#' as strings.
#' @param x The object from which to retrieve objects
setGeneric(
  "get_units",
  function(object, ...) standardGeneric("get_units")
)

#' Get string (=NA) representing units of a single non-unitbundle
setMethod(
  "get_units", "ANY",
  function(object) {
    return(NA)
  }
)

#' Get string representing single unitbundle
setMethod(
  "get_units", "unitbundle",
  function(object) {
    merge_units(list(object@unitdf))[[1]]
  }
)


#### Ops ####

setMethod(
  "Arith", signature(e1 = "unitbundle", e2 = "unitbundle"),
  function(e1, e2) {
    #print("Arith on e1=unitbundle, e2=unitbundle")
    if(nargs() == 1) {
      # Unary operators are +, -, and !
      # No action necessary
      return(e1)
    } else {
      # "+", "-", "*", "/", "^", "%%", "%/%"
      require_e2_units <- function(required.units) {
        if(e2 != required.units) {
          stop("Units of e2 are invalid in 'e1 ",.Generic,
               " e2'. Expected '",get_units(required.units),
               "', found '",get_units(e2),"'")
        }
      }
      return(switch(
        .Generic,
        "+"=, "-"= { require_e2_units(e1); e1 },
        "*"= { unitbundle(rbind(e1@unitdf, e2@unitdf)) },
        "/"= { unitbundle(rbind(e1@unitdf, transform(e2@unitdf, Power=Power*-1))) },
        "^"= { require_e2_units(unitbundle()); NA }, # can't return new units (would need values of e2's data), so return NA
        "%%"= { e1 },
        "%/%"= { unitbundle(rbind(e1@unitdf, transform(e2@unitdf, Power=Power*-1))) }
      ))
    }
  }
)

setMethod(
  "Compare", signature(e1 = "unitbundle", e2 = "unitbundle"),
  function(e1, e2) {
    #print("Compare on e1=unitbundle, e2=unitbundle")
    # "==", ">", "<", "!=", "<=", ">="
    return(switch(
      .Generic,
      "=="= { isTRUE(all.equal(e1@unitdf, e2@unitdf, check.attributes=FALSE)) },
      "!="= { !isTRUE(all.equal(e1@unitdf, e2@unitdf, check.attributes=FALSE)) },
      ">"=, "<"=, ">="=,"<="= { stop("Comparators >, <, >=, and <= are undefined for unitbundles") }
    ))
  }
)

setMethod(
  "Logic", signature(e1 = "unitbundle", e2 = "unitbundle"),
  function(e1, e2) {
    print("Logic on e1=unitbundle, e2=unitbundle")
    # "&", "|"
    stop("logic not yet implemented")
    return()
  }
)

setMethod(
  "Ops", signature(e1 = "unitbundle", e2 = "ANY"),
  function(e1, e2) {
    #print("Operating on e1=unitbundle, e2=ANY")
    if(.Generic == "^") {
      if(length(e2) != 1) {
        stop("Attempting to raise units to a power of length != 1. ",
             "Use a more flexible unitted object to accommodate the differing units that will result.")
      }
      e1@unitdf[[2]] <- e1@unitdf[[2]] * e2
      return(unitbundle(e1@unitdf))
    } else {
      return(callGeneric(e1, e2=new("unitbundle")))
    }
  }
)

setMethod(
  "Ops", signature(e1 = "ANY", e2 = "unitbundle"),
  function(e1, e2) {
    #print("Operating on e1=ANY, e2=unitbundle")
    callGeneric(e1=new("unitbundle"), e2)
  }
)

setMethod(
  "Ops", signature(e1 = "unitbundle", e2 = "list"),
  function(e1, e2) {
    #print("Operating on e1=unitbundle, e2=list")
    lapply(e2, FUN=function(e2elem) {
      .Generic = .Generic #because callGeneric will only look 1 frame up for .Generic
      callGeneric(e1, e2elem)
    })
  }
)

setMethod(
  "Ops", signature(e1 = "list", e2 = "unitbundle"),
  function(e1, e2) {
    #print("Operating on e1=list, e2=unitbundle")
    lapply(e1, function(e1elem) {
      .Generic = .Generic #because callGeneric will only look 1 frame up for .Generic
      callGeneric(unitbundle(e1elem), e2)
    })
  }
)
