## Unitted arithmetic

#### Ops ####

#' Group "Ops" functions
#' 
#' Implements the S4 group generic, Ops
#' 
#' @param e1 The first argument to a binary arithmetical operation, or the only argument to a unary operation
#' @param e2 The second argument to a binary arithmetical operation
#' @param ... Other arguments passed to the specific Ops function
setMethod("Ops", c("unitted","unitted"), function(e1, e2) {
  unitted_Ops(.Generic, e1, e2)
})

setMethod("Ops", c("unitted","ANY"), function(e1, e2) {
  unitted_Ops(.Generic, e1, unitted(e2, NA))
})

setMethod("Ops", c("ANY","unitted"), function(e1, e2) {
  unitted_Ops(.Generic, unitted(e1, NA), e2)
})


Ops.unitted <- function(e1, e2) {
  if(missing(e2)) {
    # Unary operators are +, -, and !
    # No unit checks necessary
    eout <- unitted(
      do.call(.Generic, list(deunitted(e1))),
      .get_units(e1))
    return(e1)
  } else {
    stop("Did not expect to ever see a binary operation in Ops.unitted")
  }
}

#' The workhorse method for unitted Ops.
setGeneric(
  "unitted_Ops", 
  function (.Generic, e1, e2) {
    standardGeneric("unitted_Ops")
  }
)
setMethod(
  "unitted_Ops", c("ANY","ANY"),
  function (.Generic, e1, e2) {
    if(is.atomic(e1)) {  # vectors, matrices, and arrays
      is_unary <- nargs() == 1
      if(is_unary) {
        # Unary operators are +, -, and !
        # No unit checks necessary
        eout <-
          unitted(do.call(.Generic, list(deunitted(e1))),
                  .get_units(e1))
        return(e1)
      } else {
        # Group "Ops":
        #   "+", "-", "*", "/", "^", "%%", "%/%"
        #   "&", "|", "!"
        #   "==", "!=", "<", "<=", ">=", ">"
        # where %% indicates x mod y and %/% indicates integer division
        
        # Check old units and/or set new ones
        return(switch(
          .Generic,
          "+"=, "-"=, "*"=, "/"=, "%%"=, "%/%"= { 
            unitted(do.call(.Generic, list(deunitted(e1), deunitted(e2))),
                    do.call(.Generic, list(.get_units(e1), .get_units(e2)))) }, 
          "^"= { 
            .get_units(unitted(e1, NA)) ^ .get_units(unitted(e2, NA)) # units check only
            unitted(
              do.call(.Generic, list(deunitted(e1), deunitted(e2))),
              .get_units(unitted(e1, NA)) ^ deunitted(e2)) },
          
          "&"=, "|"=, { 
            if(.get_units(e1) != .get_units(e2)) {
              warning("Units mismatch in logical operation 'e1 (",get_units(e1),") ",.Generic," e2 (",get_units(e2),")'")
            }
            do.call(.Generic, list(deunitted(e1), deunitted(e2))) },
          
          "==" = { (.get_units(e1) == .get_units(e2)) & (deunitted(e1) == deunitted(e2)) },
          "!=" = { (.get_units(e1) != .get_units(e2)) | (deunitted(e1) != deunitted(e2)) },
          "<"=, "<="=, ">"=, ">="= { 
            if(.get_units(e1) != .get_units(e2)) {
              stop("Units mismatch in comparison 'e1 (",get_units(e1),") ",.Generic," e2 (",get_units(e2),")'") 
            } else {
              do.call(.Generic, list(deunitted(e1), deunitted(e2)))
            }}
        ))
      }
    }
  }
)
# 
# setMethod(
#   "unitted_Ops", c("data.frame","data.frame")
#   function(e1, e2) {
#     if(is.data.frame(e1) | is.data.frame(e2)) {
#       # If eX is a nonlist, nondataframe, nonscalar, then replicate it as does
#       # Ops.data.frame BUT PRESERVE UNITS
#       repair_nonscalar <- function(eX, eY) {
#         if(!(!is.null(eX) && is.list(eX))) { # !isList
#           if(length(eX) > 1) { # !scalar
#             return(lapply(
#               split(rep_len(as.vector(eX), prod(dim(eY))),
#                     rep.int(seq_len(ncol(eY)),
#                             rep.int(nrow(eY), ncol(eY)))),
#               function(excol) {
#                 u(excol, .get_units(u(eX,NA)))
#               }))
#           }
#         }
#         return(eX)
#       }
#       # with the exception of this repair_nonscalar stuff, the basic procedure is 
#       # to call the generic on the data.frame and other operator and hope/expect 
#       # that units will be properly propagated through subsequent calls to
#       # Ops.unitted for the individual data columns.
#       eout <- u(do.call(.Generic, list(
#         .remove_unitted_class(repair_nonscalar(e1,e2)), 
#         .remove_unitted_class(repair_nonscalar(e2,e1)))))
#       
#       # And return
#       return(eout)
#     } 
#   }
# )

#### Math ####

#' Group "Math" functions
#' 
#' Implements the S3 group generic, Math
#' 
#' @param x A vector (probably numeric or complex)
#' @param ... Other arguments passed to the specific Math function
#' @param check.units logical. Should the units of x be checked for 
#'   compatibility with the specific Math function? Functions abs, floor, 
#'   ceiling, trunc, round, signif, and sqrt accept any units. Functions exp,
#'   log, expm1, log1p, acos, asin, atan require that inputs are unitless.
#'   Functions cos, sin, and tan require that inputs are "radians". 
Math.unitted <- function (x, ..., check.input.units=TRUE)
{
  x[] <- NextMethod(.Generic, ...) # let any other error checks happen first
  if(is.atomic(x)) { # applies to vectors, matrices, and arrays    
    units_in_out <- switch(
      .Generic,
      "abs"=, "floor"=, "ceiling"=, 
      "trunc"=, "round"=, "signif"=,
      "cumsum"=, "cummax"=, "cummin"=    c(   NA,     1), # inputs=anything, outputs=inputs
      "sqrt"=                            c(   NA,   1/2), # inputs=anything, outputs=inputs^1/2
      "sign"=                            c(   NA,    ""), # inputs=anything, outputs=unitless
      "exp"=, "log"=, "expm1"=, 
      "log1p"=, "cumprod"=               c(   "",    ""), # inputs=unitless, outputs=unitless
      "cos"=, "sin"=, "tan"=             c("rad",    ""), # inputs=radians,  outputs=unitless
      "acos"=, "asin"=, "atan"=          c(   "", "rad"), # inputs=unitless, outputs=radians
      "cosh"=, "sinh"=, "tanh"=          c("rad",    ""), # inputs=radians,  outputs=unitless # units not entirely sure from docs, but seems likely
      "acosh"=, "asinh"=, "atanh"=       c(   "", "rad"), # inputs=unitless, outputs=radians  # units not entirely sure from docs, but seems likely
      "lgamma"=, "gamma"=, "digamma"=, 
      "trigamma"=,                       c(   NA,     1)  # inputs=anything, outputs=inputs   #includes default (note comma after "trigamma"=)
      
    )
    if(isTRUE(check.input.units) & !is.na(units_in_out[1])) {
      if(!all.equal(.get_units(x), unitbundle(units_in_out[1]))) {
        stop("Input units are invalid in ", .Generic, "; found '",get_units(x),"'', expected '",units_in_out[1],"'. To override, set check.input.units to FALSE.")
      }
    }
    if(is.numeric(units_in_out[2])) {
      new_units <- attr(x,"unitdf")
      new_units$Power <- new_units$Power * units_in_out[2]
      new_units <- list(new_units)
    } else {
      new_units <- parse_units(units_in_out[2])
    }
    x <- .set_units(x, new_units)
  }
  x
}
