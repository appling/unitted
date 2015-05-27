# Package a set of units (e.g., the numerator and denominator units for a single
# vector) into a single S4 class that can be smart about reading in units
# strings, doing mathematical operations, etc.

#### Class Definition ####

#' Class unitbundle
#' 
#' A unitbundle is an object containing information about a single set of units 
#' - for example, \eqn{kg*m^2*s^-2}{kg m^2 s^-2} or \eqn{mi*hr^-1}{mi hr^-1}. 
#' This information is stored in a way that permits efficient units conversion 
#' and arithmetic.
#' 
#' @import methods
#' @name unitbundle-class
#' @rdname unitbundle-class
#' @export unitbundle
#' @exportClass unitbundle
#' @seealso \code{\linkS4class{unitted}} for data with unitbundles attached; 
#'   \code{\link{unitbundle_ops}} for arithmetic and other operations on
#'   unitbundles; \code{\link{get_units}} for conversion to character formats
setClass(
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

#' Create a unitbundle
#' 
#' There are several ways to create a unitbundle, all of which use the standard 
#' constructor \code{unitbundle(...)}. Exactly one or zero arguments from 
#' [\code{units}, \code{unitstr}, \code{unitdf}, or \code{unitbdl}] may be 
#' specified; the choice or argument determines which construction method is 
#' used. If the \code{units} argument is specified, with or without the 'units' 
#' label, then the class of that argument determines whether it is interpreted 
#' as a \code{unitdf} argument (if it's a data.frame), a \code{unitstr} argument
#' (if it's a character), or a \code{unitbdl} argument (if it's a unitbundle).
#' 
#' The basic content of a unitbundle is a collection of units each raised to a 
#' power (see Definitions below). For example, \code{"kg ha^-1"} has two units, 
#' \code{"kg"}, and \code{"ha"}, raised to the powers \code{1} and \code{-1}, 
#' respectively. In creating a unitbundle from a character string, the caret 
#' symbol (\code{^}) is used to indicate the separation between the unit and its
#' power, and a space usually indicates the separation between two unit-power 
#' pairs.
#' 
#' Character strings are a potentially ambiguous way to describe units 
#' information; in particular, a unit might itself contain a space or \code{^}. 
#' To specify the contents of a unitbundle without ambiguity, a \code{delimiter}
#' may be used to indicate where the text part of a unit begins and ends. For 
#' example, \code{"g soil^-1"} will be incorrectly parsed into \code{"g"} and 
#' \code{"soil^-1"}, but this can be avoided by wrapping the two-word unit in 
#' the delimiter: when \code{delimiter="|"}, then \code{"|g soil|^-1"} will be
#' parsed such that \code{"g soil"} is a single unit.
#' 
#' @section Definitions:
#'   
#'   \describe{
#'   
#'   \item{unit}{A single property or thing: \eqn{kg}, \eqn{tree*house}{tree
#'   house}, \eqn{"kgC_organic"}{kgC_organic}}
#'   
#'   \item{power}{The power to which a unit is raised; assumed to be 1 if 
#'   unspecified. \eqn{1}, \eqn{0.53}, and \eqn{1/2} are powers in unit-power 
#'   pairs such as \code{"apples^1"} or \code{"apples"}, 
#'   \code{"frequency^0.53"}, and \code{"acre^1/2"}}
#'   
#'   \item{unit string}{A combination of units and powers used to describe data:
#'   \code{"kgC ha^-1 yr^-1"}}
#'   
#'   \item{vector of unit strings}{\code{c("kgC ha^-1 yr^-1", "PgN", "ft^3 
#'   s^-1")}}
#'   
#'   }
#'   
#' @name Constructor
#' @rdname unitbundle-class
#' @export
#' @param units A data.frame (unitdf method), character (parse string method), 
#'   or unitbundle (replica method). Although the code is cleaner, it is 
#'   slightly less efficient to use this units argument than to explicitly 
#'   specify unitdf, unitstr, or unitbdl.
#' @param delimiter A single-character string, relevant only when \code{units} 
#'   or \code{unitstr} is a character string, used in parsing the units string 
#'   into a bundle of individual units. For example, the string "tree house^2" 
#'   would be parsed as two units, "tree" and "house"^2, but the default 
#'   delimiter of "|" causes the string "|tree house|^2" to be parsed as one 
#'   squared unit, "tree house"^2.
#' @param ... Other arguments passed to initialize
#' @param unitdf A data.frame with columns Unit=character, Power=numeric, 
#'   specifying each unit-power pair in a separate row.
#' @param unitstr A single character string containing one or more units to be 
#'   parsed and bundled, e.g. \code{"kg ha^-1 yr^-1"}
#' @param unitbdl A unitbundle. If this is supplied, the new unitbundle is 
#'   simply a replica of unitbdl.
#' @return An initialized \code{unitbundle}
#' @examples
#' unitbundle()
#' unitbundle("kg ha^-1 yr^-1")
#' unitbundle(units="kg ha^-1 yr^-1")
#' unitbundle(unitstr="kg ha^-1 yr^-1")
#' unitbundle(unitdf=data.frame(Unit=c("kg","ha","yr"), Power=c(1,-1,-1)))
#' unitbundle(unitbdl=unitbundle("kg ha^-1 yr^-1"))
unitbundle <- function(units, delimiter="|", ..., unitdf, unitstr, unitbdl) {
  argchoice <- c("units","unitdf","unitstr","unitbdl")[which(!c(missing(units), missing(unitdf), missing(unitstr), missing(unitbdl)))]
  if(length(argchoice) != 1) {
    if(length(argchoice) < 1) {
      # unitbundle() returns empty units for internal convenience; u() will still require a units specification
      return(new("unitbundle"))
    } else if(length(argchoice) > 1) {
      stop("too many units specifications; need exactly one of units, unitdf, unitstr, or unitbdl")
    }
  }
  
  switch(
    argchoice,
    units = {
      # Determine type of call based on class(units), but check for NA before 
      # switching on class(units) because NA can have any class, and check for
      # S4 before NA to avoid warnings when units is a unitbundle.
      msg <- "invalid class for units specification - must be data.frame, character, NA, unitbundle, or missing"
      if(isS4(units)) {
        if(is(units, "unitbundle")) {
          units
        } else {
          stop(msg)
        }
      } else if(isTRUE(is.na(units))) {
        new("unitbundle")
      } else {
        switch(
          class(units),
          data.frame = unitbundle(unitdf=units, ...),
          character = unitbundle(unitstr=units, delimiter=delimiter, ...),
          stop(msg)
        )
      }
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
      new("unitbundle", unitdf=sort_units(simplify_units(list(unitdf)))[[1]], ...) 
    },
    unitstr = {
      if(length(unitstr) != 1) {
        stop("a character argument describing a units bundle must have length 1")
      }
      new("unitbundle", unitdf=sort_units(simplify_units(parse_units(unitstr, delimiter=delimiter)))[[1]], ...) 
    },
    unitbdl = {
      unitbdl
    }
  )
}

#### Display ####

setMethod(
  "show", "unitbundle", 
  function(object){
    cat("unitbundle:",get_units(object))
  }
)


#### Inspect ####

#' Gets the units as a string or strings.
#' 
#' Units can be acquired from objects of all types; types without units will
#' return NA.
#' 
#' @exportMethod get_units
#' @rdname get_units
#' @param object The object from which to retrieve units
#' @param ... Other arguments passed on to the type-specific implementations
setGeneric(
  "get_units",
  function(object, ...) standardGeneric("get_units")
)

#' @rdname get_units
#' @examples
#' get_units(5)
setMethod(
  "get_units", "ANY",
  function(object, ...) {
    return(NA)
  }
)

#' @rdname get_units
#' @param delimiter A single-character string designating the delimiter that 
#'   should surround those units that are to be delimited according to 
#'   \code{rule}
#' @param rule character string indicating the rule by which each 
#'   unit within a unitdf will be delimited or not. The default, "disambiguate",
#'   wraps only those units that contain spaces or "^" characters. "never" wraps
#'   none, and "always" wraps all.
#' @examples
#' get_units(unitbundle("king kong"))
setMethod(
  "get_units", "unitbundle",
  function(object, delimiter="|", rule=c("disambiguate","never","always"), separator=" ", ...) {
    merge_units(list(object@unitdf), delimiter=delimiter, rule=rule, separator=separator)[[1]]
  }
)

#' Separate a unitbundle into a data frame of units and powers
#' 
#' @export
#' @param unitbds a unitbundle
#' @return a data.frame with columns for unit and power
separate_units <- function(unitbdl) {
  unitbdl@unitdf
}


#### Ops ####

#' Operations on unitbundles
#' 
#' When data with units are combined by arithmetic operations, the units ought 
#' to be propagated through according to standard rules. Data that are 
#' \code{unitted} obey these rules. To make this possible, the units themselves 
#' (as \code{unitbundle} objects) also obey rules relevant to units propagation,
#' but be warned - the rules are slightly different for \code{unitbundle} 
#' objects than they are for \code{unitted} objects. See \strong{Details | 
#' Arithmetic on unitbundles}.
#' 
#' The rules for operations on unitbundles are described in the following
#' subsections.
#' 
#' \subsection{Arithmetic on unitbundles}{
#' 
#' \describe{
#' 
#' \item{+e1, -e1, !e1}{Returns the original units of e1, which are unchanged by
#' these unary operations.}
#' 
#' \item{e1+e2, e1-e2}{e1 and e2 are required to contain identical units (an 
#' error is thrown otherwise); the units of e1 (or, equivalently, e2) are 
#' returned unaltered.}
#' 
#' \item{e1*e2}{Returns a new unitbundle containing the union (product) of all 
#' units in e1 and e2.}
#' 
#' \item{e1/e2}{Returns a new unitbundle with the units of e1 in the numerator 
#' and the units of e2 in the denominator.}
#' 
#' \item{e1^e2}{Requires that e2 contain no units; the new units must be 
#' calculated with respect to the data attached to e2 but cannot be calculated 
#' here because the data are unavailable to unitbundles themselves. The units of
#' e1 are returned.}
#' 
#' \item{e1\%\%e2}{Returns the units of e1 regardless of the units of e2.}
#' 
#' \item{e1\%/\%e2}{Returns the same units as e1/e2.}
#' 
#' }
#' 
#' }
#' 
#' @name unitbundle_ops
#' @rdname unitbundle_ops
#' @export
#' @seealso \code{\link{unitbundle}} for the \code{unitbundle} class; 
#'   \code{\linkS4class{unitted}} for data with unitbundles attached
#' @family unitbundle manipulation
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

#' Comparisons of unitbundles
#' 
#' When data with units are compared, the units ought 
#' to be considered according to standard rules. Data that are 
#' \code{unitted} obey these rules. To make this possible, the units themselves 
#' (as \code{unitbundle} objects) also obey rules relevant to comparison,
#' but be warned - the rules are slightly different for \code{unitbundle} 
#' objects than they are for \code{unitted} objects. See \strong{Details |
#' Comparisons of unitbundles}.
#' 
#' \subsection{Comparisons of unitbundles}{
#' 
#' \describe{
#' 
#' \item{e1==e2}{True if e1 and e2 are identical.}
#' 
#' \item{e1!=e2}{True if e1 and e2 are anything but identical.}
#' 
#' \item{>, <, >=, <=}{Undefined for unitbundles.}
#' 
#' }
#' 
#' }
#' 
#' @name unitbundle_comparison
#' @rdname unitbundle_ops
#' @export
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

#' Logical operations on unitbundles
#' 
#' It is not clear what it would mean to perform logical operations on
#' unitbundles; for this reason, methods in the \code{Logic} group throw errors
#' when applied to unitbundles.
#' 
#' @name unitbundle_logic
#' @rdname unitbundle_ops
#' @export
setMethod(
  "Logic", signature(e1 = "unitbundle", e2 = "unitbundle"),
  function(e1, e2) {
    print("Logic on e1=unitbundle, e2=unitbundle")
    # "&", "|"
    stop("unitbundle logic is not implemented")
    return()
  }
)

#' Operations combining unitbundles and non-unitbundles
#' 
#' Arithmetic and comparison operations can sometimes be done on a unitbundle 
#' and an object of another type. This is most important for raising a 
#' unitbundle e1 to a power e2, which actually \emph{requires} that e2 be a 
#' number, not a unitbundle. Another useful feature of unitbundle operations is 
#' that operation on a unitbundle and a list is conducted elementwise over the 
#' entire list. See \strong{Details | Operations combining unitbundles and 
#' non-unitbundles}.
#' 
#' \subsection{Operations combining unitbundles and non-unitbundles}{
#' 
#' \describe{
#' 
#' \item{\code{Ops(e1,e2)} - default}{In general, if one of e1 and e2 is a
#' unitbundle and the other is not, the non-unitbundle is replaced by an empty
#' unitbundle and the operation proceeds.}
#' 
#' \item{\code{Ops(e1,e2)} - lists}{If the non-unitbundle is a list, the
#' operation is performed on the unitbundle and each element of the list.}
#' 
#' \item{e1^e2}{If e1 is a unitbundle and e2 is numeric, the units in e1 are 
#' raised to the power in e2.}
#' 
#' }
#' 
#' }
#' 
#' @name unitbundle_any_ops
#' @rdname unitbundle_ops
#' @export
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

#' @name any_unitbundle_ops
#' @rdname unitbundle_ops
#' @export
setMethod(
  "Ops", signature(e1 = "ANY", e2 = "unitbundle"),
  function(e1, e2) {
    #print("Operating on e1=ANY, e2=unitbundle")
    callGeneric(e1=new("unitbundle"), e2)
  }
)

#' @name unitbundle_list_ops
#' @rdname unitbundle_ops
#' @export
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

#' @name list_unitbundle_ops
#' @rdname unitbundle_ops
#' @export
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
