#### c ####

#' Combine unitted elements into a unitted vector
#' 
#' This function takes one or more unitted elements and combines them into a 
#' single unitted vector. To enforce units integrity, the function requires that
#' all elements passed to the S3 or S4 versions of c.unitted() must have the
#' same units, at least when recursive=FALSE. When recursive=TRUE, elements may
#' include lists or data.frames, but the base elements (vectors, columns, etc.)
#' of those list elements must still be unitted.
#' 
#' The S4 method for c<<unitted>> will be found first (before other c() methods)
#' whenever the first argument is unitted.
#' 
#' @name unitted_c
#' @family unitted object manipulation
#' @return A unitted vector
NULL

#' @rdname unitted_c
#' @inheritParams base::c
#' @param x the first argument passed to \code{c}
#' @export
setMethod(
  "c", "unitted",
  function(x, ..., recursive=FALSE) {
    if(missing(x)) return(NULL)
    listarg <- c(list(x),list(...))
    
    # decide on the type of join to do
    ctype <- if(any(sapply(listarg, function(elem) isTRUE(is(elem, "unitted_list"))))) { "unitted_list" 
    } else if(any(sapply(listarg, function(elem) isTRUE(is.list(elem))))) { "list or data.frame"
    } else { "atomic" }
    
    if(ctype == "atomic") {
      # If all elements are atomic, (1) check to make sure they all have the
      # same units, (2) combine them using the usual c(), and (3) assign the
      # resulting atomic thing the single common set of units.
      newunits <- unique(get_units(listarg, recursive=TRUE))
      if(length(newunits) != 1) {
        stop("every element must have the same units")
      }
      vlist <- lapply(listarg, deunitted)
      unitted(do.call("c", vlist), newunits)
      
      # If the elements include a list (including data.frame), (1) convert each 
      # element to a list using as.list, (2) join the lists, and (3) don't 
      # assign units; any useful units will be attached to the individual 
      # elements of the resulting list.
      
      # If the elements include a data.frame, (1) convert the dataframe to a
      # list, (2) convert any other elements to lists using as.list
      # (push.units=TRUE), (3) combine into a single list, and (4) don't assign
      # units; any useful units will be attached to the individual elements of
      # the resulting list.
    } else if(ctype == "list or data.frame") {
      # push.units is ignored by most as.list calls and is TRUE by default for unitted vectors& lists
      vlist <- lapply(listarg, function(elem) as.list(elem) )
      do.call("c", vlist)
      
      # If the elements include a unitted_list, then call as.list with 
      # push.units=TRUE and do the list join with the requirement that all lists
      # have the same units. Return a single unitted_list.
    } else if(ctype == "unitted_list") {
      newunits <- unique(get_units(listarg, recursive=TRUE))
      if(length(newunits) != 1) {
        stop("every element must have the same high-level units")
      }
      vlist <- lapply(listarg, deunitted, partial=TRUE)
      unitted(do.call("c", vlist), newunits)  
    }   
  }
)

#' If the first argument is not unitted but later arguments are, then the 
#' primitive c method will still be called, and unitted objects will be coerced 
#' to their S3 data parts. So if you want to maintain units through a call to 
#' \code{c()} where the first argument is not unitted, then you should call 
#' \code{c.unitted} directly.
#' 
#' To avoid repeating code, the S3 function simply calls the S4 method.
#' 
#' @rdname unitted_c
#' @export
c.unitted <- function(..., recursive=FALSE) {
  getMethod("c","unitted")(..., recursive=recursive)
}

#### .unitted_bind ####

# Main implementation of rbind and cbind for unitted objects
# 
# The most reliable way to call these is with rbind.unitted() and 
# cbind.unitted(). Although rbind() and cbind() often work correctly for 
# combinations of two or more unitted objects of the same class (e.g., two 
# unitted_data.frames or two unitted_characters), the base rbind and cbind 
# methods often fail to redirect to their unitted versions when there are type 
# mismatches, even minor ones, among the objects to be bound.
# 
# Known issues:
# 
# cbind/rbind(unitteddataframe, dataframe) or cbind/rbind(dataframe, 
# unitteddataframe) does not get routed to these unitted methods; your options 
# in this case are to call c/rbind(unitteddataframe, u(dataframe)) or to call 
# c/rbind.unitted(unitteddataframe, dataframe).
# 
# The unitted cbind and rbind do not implement deparse.level=2 -- this would
# require too much duplication of the base rbind and cbind code for not enough
# reward.
.unitted_bind <- function(..., fun.name=c("cbind","rbind")) {
  # Our basic strategy is to create local, deunitted copies of each element to be
  # bound. It helps for these copies to have the same names as the originals (for
  # deparse.level = 1 in particular); if the originals were given as expressions,
  # however, we need to rename them to a local variable name so that we can store
  # their deunitted values. The argument TAG names (as opposed to the variable 
  # names) should be kept as in the original call, again to permit rbind(..., 
  # deparse.level=1) to work its magic. Further, arguments that were orignally 
  # expressions should again be passed to rbind as expressions, except that these
  # new expressions should evaluate to deunitted objects.
  
    
  # Get a list of the arguments in ..., names and values
  evaluated_args <- list(...)
  
  # Figure out where the original rbind calling frame is relative to this one;
  # it depends on whether we arrived here via regular rbind (calling_frame=-2)
  # or by a direct call to rbind.unitted[_something] (calling_frame=-1). Assume
  # the user didn't call .unitted_bind directly, because they shouldn't.
  sys_calls <- as.character(sys.calls())
  calling_frame <- ifelse(isTRUE(paste0(fun.name,"(deparse.level, ...)") %in% sys_calls), -2, -1)
  
  # Find, clean, and check deparse.level. rbind and cbind don't pass on 
  # deparse.level to any specialized bind functions, including our unitted ones.
  # So we have to look back through the call stack. This works if the 
  # deparse.level was passed to rbind or cbind as a value. But if it was passed
  # as a symbol, we're out of luck and need to give a warning and make an
  # assumption.
  deparse.level <- sys.call(calling_frame)[["deparse.level"]]
  if(is.null(deparse.level)) {
    # deparse.level=1 is the default to rbind/cbind, though it's hidden from us
    # by their funny function dispatch
    deparse.level <- 1L 
  } else if(is.symbol(deparse.level)) {
    warning("deparse.level passed to unitted ",fun.name," as an expression cannot be read; assuming deparse.level=1")
    deparse.level <- 1L
  } else {
    deparse.level <- as.integer(deparse.level)
  }
  stopifnot(0 <= deparse.level, deparse.level <= 2)
  if(deparse.level == 2) {
    warning("deparse.level=2 has no unitted implementation; defaulting to deparse.level=1")
    deparse.level <- 1L
  }
  
  # Inspect the arguments passed to the rbind/cbind call in the calling_frame. 
  # Often, args_to_bind will contain the original expressions used to define the 
  # arguments, in which case we can pass these on to rbind or cbind, where they 
  # may be used to name the rows or columns (depending on deparse.level). 
  # However, users could possibly call rbind() from within another function, 
  # passing along arguments using "...". In this case, I believe we can't do 
  # better than deparse.level=0, because I don't know how to reliably identify 
  # the expressions passed into the first "...".
  if(deparse.level > 0L) {
    args_to_bind <- as.list(sys.call(calling_frame)[-1L])
    # We're in trouble (re: deparsing) if and only if there's a '...' in args_to_bind
    if(any(sapply(args_to_bind, 
                  function(arg) {
                    if(is.symbol(arg)) arg == as.symbol("...") else FALSE
                  }))) {
      warning("arguments could not be deparsed; forcing deparse.level to 0")
      deparse.level <- 0L
    }
  }
  
  # We should have reduced the deparse.level possibilities to 1L or 0L by now
  stopifnot(deparse.level %in% c(0L, 1L))
  
  # Come up with a list of the elements (in one form or another, depending on
  # deparse.level) to be bound together. Consider any argument not named "deparse.level".
  if(deparse.level == 0L) { 
    # (We're checking deparse.level again because it might have changed just above.)
    args_to_bind <- evaluated_args
  }
  if(!is.null(names(args_to_bind))) {
    args_to_bind <- args_to_bind[which(names(args_to_bind) != "deparse.level")]
  }
  
  # Return NULL if it's just that easy
  if(length(args_to_bind) == 0) return(NULL)
  
  # Now assign each argument a variable name in which the deunitted copy will be
  # stored, and a symbol or expression with which to call it in our upcoming 
  # call to non-unitted rbind/cbind. 
  arg_names <- character(length(args_to_bind))
  if(deparse.level == 0L) {
    # Create a placeholder symbol in which to store the deunitted copy of each arg
    for(i in seq_along(args_to_bind)) {
      arg_names[i] <- paste0("XX",i,"XX")
      args_to_bind[[i]] <- substitute((a),list(a=as.symbol(arg_names[i]))) # wrap the name in () so it's called as an expression
    }
  } else {
    # Replace any non-symbols (i.e., expressions) with placeholder symbols, like we did for deparse.level==0 just above
    for(i in seq_along(args_to_bind)) {
      arg <- args_to_bind[[i]]
      if(is.symbol(arg)) {
        arg_names[i] <- as.character(arg)
        # Check to make sure we won't be overwriting another argument; this
        # check is a touch conservative, but conflicts should still be rare
        if(grepl("XX(.*)XX", arg_names[i])) {
          stop("sorry - symbols passed to unitted ",fun.name," can't have the pattern XX*XX")
        }
        # args_to_bind[[i]] <- arg # already done
        } else {
        arg_names[i] <- paste0("XX",i,"XX") # this could be a problem if one of the args is already XX##
        args_to_bind[[i]] <- substitute((a),list(a=as.symbol(arg_names[i]))) # wrap the name in () so it's called as an expression
      }
    }
  }
  
  # Evaluate each argument, collect its units, and put a deunitted version in
  # an empty environment called v_frame
  newunits <- list()
  v_frame <- new.env()
  for(i in seq_along(args_to_bind)) {
    newunits[[i]] <- get_unitbundles(evaluated_args[[i]])
    assign(arg_names[i], deunitted(evaluated_args[[i]]), pos=v_frame)
  }
        
  if(fun.name=="cbind" & any(sapply(evaluated_args, is.data.frame))) {
    newunits <- unlist(newunits, recursive=FALSE)
  } else {
    # In most cases - rbind, or cbind for non-data.frames - if there's more than
    # one unit, we shouldn't be binding.
    newunits <- unique(newunits)
    if(length(newunits) > 1) {
      # Compare the remaining (unique) newunits to see if they're equivalent
      # according to ==.unitbundle. In particular, unitbundle("") == NA
      error_message <- "Every element must have the same units"
      for(nu1 in seq_len(length(newunits)-1)) {
        for(nu2 in (nu1+1):length(newunits)) {
          if(is.list(newunits[[nu1]])) { # usually data.frames
            if(length(newunits[[nu1]]) != length(newunits[[nu2]])) {
              stop(error_message)
            } else {
              for(nu_elem in seq_along(newunits[[nu1]])) {
                if(newunits[[nu1]][[nu_elem]] != newunits[[nu2]][[nu_elem]]) {
                  stop(error_message)
                }
              }
            }
          } else {
            if(newunits[[nu1]] != newunits[[nu2]]) {
              stop(error_message)
            }
          }
        }
      }
    }
    newunits <- newunits[[1]]
  }
  
  # Add deparse.level to the list of arguments to be send back to rbind/cbind
  assign("deparse.level", deparse.level, pos=v_frame)
  args_to_bind <- c(args_to_bind, list(deparse.level=substitute(deparse.level)))
    
  # Evaluate rbind or cbind in the context of the deunitted copies in v_frame
  bound <- do.call(fun.name, args_to_bind, envir=v_frame)
  unitted(bound, newunits)
}

#### rbind ####

#' Bind unitted objects by row or column
#' 
#' Combines unitted objects as if they were not unitted, but enforces unit
#' consistency across the objects to be combined.
#' 
#' \code{deparse.level} is an integer controlling the construction of labels, as in 
#'   the default rbind and cbind methods. Only deparse.level = 0 and 1 are 
#'   available for unitted rbind/cbind calls.
#'   
#' @name unitted_bind
#' @aliases unitted_rbind rbind bind
#' @rdname unitted_bind
#' @export
#' @family unitted object manipulation
#' 
#' @param ... unitted vectors, matrices, or data.frames
rbind.unitted <- function(...) {
  .unitted_bind(..., fun.name="rbind")
}

# The specific unitted_xxx functions get discovered by rbind(), while the 
# generic rbind.unitted never does. So we need an rbind function for every 
# unitted subclass. The rbind.unitted function defined above is also useful,
# however, for when someone wants to explicitly specify a unitted type of rbind.
for(subclass in names(getClass("unitted")@subclasses)) {
  assign(paste0("rbind.",subclass), function(...) {
    .unitted_bind(..., fun.name="rbind")
  })
}

#### cbind ####

#' @aliases unitted_cbind cbind
#' @rdname unitted_bind
#' @export
cbind.unitted <- function(...) {
  .unitted_bind(..., fun.name="cbind")
}

# Same logic as for rbind methods.
for(subclass in names(getClass("unitted")@subclasses)) {
  assign(paste0("cbind.",subclass), function(...) {
    .unitted_bind(..., fun.name="cbind")
  })
}


#### merge ####


setGeneric("merge")

#' Merge unitted data.frames by one or more common columns
#' 
#' Merges unitted data.frames, ensuring units compatibility among the common
#' columns
#' 
#' @name unitted_merge
#' 
#' @seealso \code{base::\link{merge}}
#' @family unitted object manipulation
NULL

#' @rdname unitted_merge
#' @param x first unitted object to merge
#' @param y second unitted object to merge
#' @param ... other arguments passed to \code{c}
#' @export
setMethod(
  "merge", 
  c(x="unitted_data.frame", y="unitted_data.frame"),
  function(x, y, ...) {
    # Do the basic merge without units; any errors will be thrown now before we get into units checks
    merged_xy <- merge(v(x), v(y), ...)
    
    # Now try to merge units
    xu <- do.call(data.frame, c(as.list(get_units(x)), list(stringsAsFactors=FALSE)))
    yu <- do.call(data.frame, c(as.list(get_units(y)), list(stringsAsFactors=FALSE)))
    dots <- list(...)
    if(length(dots) > 0) {
      dots <- dots[which(!(names(dots) %in% c("all","all.x","all.y")))]
    }
    merged_units <- do.call(merge, c(list(xu, yu, all=T),dots))
    
    # Check on merged units
    if(nrow(merged_units) != 1) {
      message("Attempting to merge with these conflicting units:")
      message(paste(capture.output(print(merged_units)),collapse="\n"))
      stop("Units conflict in merge")
    }
    if(!all(names(merged_units) == names(merged_xy))) {
      message("Names of merged data.frame:")
      message(paste(names(merged_xy),collapse=" "))
      message("Names of merged units:")
      message(paste(names(merged_units),collapse=" "))
      stop("Couldn't reconcile names of merged data.frames and merged units")
    }
    
    # If the units look good, send them out with the merged data
    unitted(merged_xy, unlist(merged_units[1,]))
  }
)

#' @rdname unitted_merge
#' @export
setMethod(
  "merge",
  c(x="unitted", y="ANY"),
  function(x, y, ...) {
    stop("merge for unitted, ANY not yet implemented")
  }
)

#' @rdname unitted_merge
#' @export
setMethod(
  "merge",
  c(x="ANY", y="unitted"),
  function(x, y, ...) {
    stop("merge for ANY, unitted not yet implemented")
  }
)


#### rep ####

#' Replicate elements, maintaining the original units
#' 
#' Wrapper for non-unitted rep() methods.
#' 
#' @name unitted_rep
#' @rdname unitted_rep
#' @param x object to replicate
#' @param ... other objects passed to \code{rep}
#' @export
#' @family unitted object manipulation
rep.unitted <- function(x, ...) {
  unitted(rep(deunitted(x), ...), get_unitbundles(x))
}

#' @rdname unitted_rep
#' @export
rep.unitted_data.frame <- function(x, ...) {
  rep(deunitted(x, partial=TRUE), ...)
  
  # less efficient, I think:
  #   mapply(
  #     function(elem, units) unitted(elem, units),
  #     rep(deunitted(x), ...),
  #     rep(get_unitbundles(x), ...),
  #     SIMPLIFY=FALSE)
}
