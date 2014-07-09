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

# Use the Ops.data.frame function to apply the Ops to the columns, which will 
# then call unitted_Ops(ANY, ANY) to check and manipulate units on a
# column-by-column basis.
setMethod("Ops", c("unitted","data.frame"), function(e1, e2) {
  #print("Ops(unitted,data.frame)")
  #UseMethod(.Generic, deunitted(unitted(e2, NA), partial=TRUE))
  #print(str(.Method))
  #with(list())
  .Method <<- c(if(is.atomic(e1)) "" else class(deunitted(e1)), "data.frame")
  .Generic <<- .Generic
  df <- Ops.data.frame(e1, deunitted(unitted(e2, NA), partial=TRUE))
  unitted(df, NA)
})
setMethod("Ops", c("data.frame","unitted"), function(e1, e2) {
  .Method <<- c("data.frame", if(is.atomic(e2)) "" else class(deunitted(e2)))
  .Generic <<- .Generic
  df <- Ops.data.frame(deunitted(unitted(e1, NA), partial=TRUE), e2)
  unitted(df, NA)
})


# We don't currently treat arrays and matrices differently from vectors, but
# here's how we'll get the right routing if and when we permit multiple units
# per array or matrix.
setMethod("Ops", c("unitted","array"), function(e1, e2) {
  # This method outcompetes structure#vector in dispatch, where ANY#unitted only ties it.
  unitted_Ops(.Generic, e1, unitted(e2, NA))
})
setMethod("Ops", c("array","unitted"), function(e1, e2) {
  # This method outcompetes structure#vector in dispatch, where ANY#unitted only ties it.
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
    is_unary <- nargs() == 1
    if(is_unary) {
      # Unary operators are +, -, and !
      # No unit checks necessary
      warning("wasn't expecting a unary argument to unitted_Ops")
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
            #mapply(function(el1, el2) { 
            #  .get_units(el1) ^ deunitted(el2) }, e1, e2, SIMPLIFY=TRUE) )},
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
)

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
  mathx <- NextMethod(.Generic, ...) # let any other error checks happen first
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
      if(!all(.get_units(x) == unitbundle(units_in_out[1]))) {
        stop("Input units are invalid in ", .Generic, "; found '",get_units(x),"', expected '",units_in_out[1],"'. To override, set check.input.units to FALSE.")
      }
    }
    if(is.numeric(units_in_out[2])) {
      new_units <- .get_units(x) ^ units_in_out[2]
    } else {
      new_units <- unitbundle(units_in_out[2])
    }
    x <- .set_units(mathx, new_units)
  }
  x
}
