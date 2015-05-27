
#### Unit conversions ####

# This function requires a data.frame called UnitsConversions. There's probably a better solution.

# uconv <- unitConversion <- function(to,from) {
#  #first locate the relevant row in the UnitsConversions data.frame
#   conversionRow <- UnitsConversions[which(UnitsConversions$FromUnits==from & UnitsConversions$ToUnits==to),]
#   flip <- FALSE
#   if(nrow(conversionRow)==0) { #try the opposite direction, to -> from 
#     conversionRow <- UnitsConversions[which(UnitsConversions$FromUnits==to & UnitsConversions$ToUnits==from),]
#     flip <- TRUE
#   }
#   if(nrow(conversionRow)==0) { #in this case we've tried both directions & found nothing
#     return(NA)
#   }
# 
#   #turn the data.frame row into a unitted value
#   conversionfactor <- u(conversionRow$Value,paste0(conversionRow$ToUnits," ",conversionRow$FromUnits,"^-1"))^ifelse(flip,-1,1)
#   
#   #if aliases have been provided by passing the arguments to and/or from as  
#   #named vectors of length 1 each, use those aliases instead of the standard 
#   #conversion factors.
#   conversionfactor <- conversionfactor *
#   u(1,ifelse(is.null(names(to)),"",paste0(names(to)," ",to,"^-1"))) *
#   u(1,ifelse(is.null(names(from)),"",paste0(from," ",names(from),"^-1"))) 
#   return(conversionfactor)
#   
# }

# A function like replace() but for unitted data.frames
unittedreplace <- function(unitteddf, colname, replacefun, replacevalues) { 
  if(is.unitted(replacevalues)) {
    if(get_units(replacevalues) != get_units(unitteddf[,colname])) {
      stop("Units of replacement value[s] conflict with units of column")
    }
  }
  u(replace(v(unitteddf[,colname]),replacefun(unitteddf[,colname]),v(replacevalues)),
    get_units(unitteddf[,colname])) 
}

#### Data type conversions ####

# as.Date defaults don't work well for unitted dates; the S4 container becomes
# Date but the internal representation stays as it was. So let's rewrite that.
as.Date.unitted <- function(x, ...) {
  as.Date(deunitted(x), ...)
}

#' as.list()
as.list.unitted <- function(x, ...) {
  .unitted_as.list(x, ...)
}
setGeneric(
  ".unitted_as.list", function (x, ...) {
    standardGeneric(".unitted_as.list")
  }
)
setMethod(
  ".unitted_as.list", "ANY",
  function(x, push.units=TRUE, ...) {
    if(push.units) {
      res <- vector("list", length(x))
      for (i in seq_along(x)) res[[i]] <- x[i]
      res
    } else {
      as.list(deunitted(x, partial=TRUE))
    }
  }
)
setMethod(
  ".unitted_as.list", "unitted_data.frame",
  function(x, push.units=NA, ...) {
    # push.units is ignored
    as.list(deunitted(x, partial=TRUE), ...)
  }
)
setMethod(
  ".unitted_as.list", "unitted_list",
  function(x, push.units=TRUE, ...) {
    #not sure if as.list(list) ever does anything special; keep it just in case
    #should be slightly faster to keep units (partial=TRUE) than not
    res <- as.list(deunitted(x, partial=TRUE), ...) 
    if(push.units) {
      ux <-  get_unitbundles(x)
      for (i in seq_along(x)) res[[i]] <- u(res[[i]], ux)
    }
    res
  }
)

#' as.unitted_list
setGeneric(
  "as.unitted_list", 
  function(x, ...) {
    standardGeneric("as.unitted_list")
  }
)
setMethod(
  "as.unitted_list", "ANY",
  function(x, ...) {
    u(as.list(deunitted(x), ...), get_units(x))
  }
)
setMethod(
  "as.unitted_list", "list",
  function(x, ...) {
    xunits <- unique(get_units(x))
    if(length(xunits) != 1) {
      stop("elements passed to as.unitted_list must all have the same units")
    }
    u(deunitted(x), get_units(x))
  }
)
setMethod(
  "as.unitted_list", "unitted_list",
  function(x, ...) {
    x
  }
)