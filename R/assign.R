#' Replace cell[s] of a unitted data.frame
#' 
#' Bug: If you want to add columns to a unitted data.frame, you have to do it
#' one at a time. This won't work: 
#' chemdat[ghgrows,c(ghgcols,"CO2aq_G_APA","CH4aq_G_APA","N2Oaq_G_APA")] <- ghgchem 
#' where "CO2aq_G_APA","CH4aq_G_APA","N2Oaq_G_APA" are new columns to
#' chemdat and where ghgchem and chemdat are both unitted already.
#' 
#' @param x The data.frame
#' @param i The row (or column, if j is missing)
#' @param j The column
#' @param value The value or values to insert in the specified cell[s]
#' @return A new data.frame with the specified cells replaced with \code{value}
#' @export
"[<-.unitted" <- function(x,i,j,...,value) {
  call_args <- as.list(match.call())
  access_args <- call_args[-c(1, 2, length(call_args))]
  
  replacee <- do.call(`[`, c(list(x), access_args))
  assigned <- NextMethod()
  #as_replaced <- do.call(`[`, c(list(assigned), access_args))
  
  msg <- function(replacee, replacement) {
    paste0("Units mismatch in partial replacement. ",
           "Replacee units: '",get_units(replacee),
           "'; replacement units: '",get_units(replacement),"'")
  }
  if(is.atomic(.remove_unitted_class(x))) {
    if(is.null(dim(x))) {
      # Vectors
      if(.get_units(replacee) != .get_units(value)) {
        stop(msg(replacee, value))
      }
    } else {
      # Matrices and arrays
      if(.get_units(replacee) != .get_units(value)) {
        stop(msg(replacee, value))
      }
    }
    
  } else if(is.data.frame(x)) {
    
  } else {
    stop("Unrecognized data type of x in [<-.unitted")
  }
  
  return(assigned)

}


#' Replace a column of a unitted data.frame
#' 
#' Returns a vector with the right units still attached.
#' 
#' @param x The data.frame
#' @param i The row (or column, if j is missing)
#' @param j The column
#' @param value The value or values to insert in the specified cell[s]
#' @return A unitted vector
#' @export
"$<-.unitted" <- function(x,col,value) {
  if(!is.data.frame(x))
    stop("The only unitted data type currently supported for $<- replacement\n",
         "is a data.frame, which this is not.")
  get("[<-.unitted")(x,,col,value=value)
  #eval(call("[<-.unitted",x,NULL,col,value))
  #x[,col] <- value
}


#' Replace values within a unitted object
#' 
#' @param x The original unitted data object
#' @param i The row (or column, if j is missing)
#' @param j The column
#' @param value The value or values to insert in the specified cell[s]
#' @return A new unitted data object
#' @export
"[[<-.unitted" <- function(x,i,j,...,value) {
  stop("not yet implemented")
}
