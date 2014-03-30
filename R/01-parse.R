# Utility functions used by unitbundle.R for turning units into strings and vice
# versa

#' parse_units turns a unit string into an unambiguous internal representation 
#' of a set of units.
#' 
#' It is vectorized so that N unit strings will be turned into N internal 
#' representations.
#' 
#' Definitions. This is a unit: "kg". This is another unit: "kgC_organic^2". 
#' This is a unit string: "kgC ha^-1 yr^-1". This is a vector of unit strings: 
#' c("kgC ha^-1 yr^-1", "PgN", "ft^3 s^-1").
#' 
#' @param ustrs character vector of unit strings, one string per set of units
#' @param delimiter single character. The delimiter can be used to identify 
#'   complex units - for example, with "|" as the delimiter, you might specify 
#'   units of "|g SO_4^2-| m^-2 hr^-1". Then "g SO_4^2-" would be considered a 
#'   single unit with power of 1. "m" and "hr" would also be recognized as 
#'   units because they are separated by spaces and contain no internal special 
#'   characters (such as spaces or "^").
#' @return list of data.frames, one per unit string passed in
parse_units <- function(ustrs, delimiter="|") {
  # In variable names below, a 'word' is a substring describing a single
  # unit^power element (or just a unit, with implied power=1).
  
  ustrs <- unname(ustrs)
  
  # q is our placeholder delimiter, to be replaced by the actual delimiter.
  # Since q appears nowhere in our regular expressions except where it's a
  # delimiter, this should always work.
  power_pattern <- "(\\^[-.[:digit:]]+)?"
  delim_pattern <- prepare_delimiter(delimiter)
  delim_element_pattern <- paste0("([q]([^q]+)[q]",power_pattern,")") # keep
  nondelim_element_pattern <- paste0("([^q[:space:]]([^[:space:]]*[^q[:space:]])?",power_pattern,")")  #"([^q[:space:]]([^[:space:]]*[^q[:space:]])?"
  word_pattern <- gsub("q", delim_pattern, paste0(delim_element_pattern,"|",nondelim_element_pattern))
  # the above construction of word_pattern could be simplified toa single gsub 
  # operation on a really complicated string, but I'm going to opt for code 
  # readability rather than efficiency for a while. at production time, we'll
  # move to code with fewer intermediate variables, assignments, and paste0
  # operations.
  word_matches <- gregexpr(word_pattern, ustrs)
  
  # in word_matches we've identified the set of words in each unit string. now 
  # parse each word into a row of a data.frame, one data.frame for each unit 
  # string, and then put the data.frames into a list exactly as long as the
  # vector of original unit strings.
  unit_dfs <- lapply(regmatches(ustrs, word_matches), function(word_vec) {
    if(length(word_vec) == 0) {
      return(data.frame(
        Unit=character(0),
        Power=numeric(0),
        stringsAsFactors=FALSE))
    }
    
    power_num_pattern <- "(\\^([-.[:digit:]]+))?"
    inner_pattern <- paste0("(([q]([^q]+)[q])|([^q[:space:]^]([^[:space:]^]*[^q[:space:]^])?))",power_num_pattern)
    # the above 2-line construction of inner_pattern could be sped up slightly
    # by defining one long, complicated regular expression. but for clarity i'll
    # keep it in several lines for now.
    wordpart_matches <- regexec(gsub("q", delim_pattern, inner_pattern), word_vec) # detect pre-^ and post-^ material (and some extra stuff)
    splitword_list <- regmatches(word_vec, wordpart_matches)
    
    # in splitword_list, each list element is a vector containing 8 pieces of a
    # single word. we'll now pick out the two useful pieces from each of these
    # vectors.
    unitpower_list <- lapply(splitword_list, function(splitword) { c(
      #gsub(gsub("q",delim_pattern,"^[q]|[q]$"),"", splitword[2])[[1]], # 2nd element is the unit; strip delimiters from its edges
      if(splitword[4L]=="") splitword[5L] else splitword[4L], # alternatively, I think, either 4th element (dleimited) or 5th element (non-delimited) is the pre-stripped unit
      if(splitword[8L]=="") 1 else splitword[8L]) # 4th element is the power; leave as character for now
    })
    
    # reshape list of 2-element vectors into 2-column matrix. sapply wraps lapply, so simplifying here bypasses unneeded error checking.
    unit_mat <- matrix(unlist(unitpower_list),ncol=2,byrow=TRUE)
    
    # convert from matrix to data.frame with correct data types
    return(data.frame(
      Unit=unit_mat[,1],
      Power=tryCatch(
        as.numeric(unit_mat[,2]), 
        warning=function(w) { 
          if(w$message == "NAs introduced by coercion") {
            w$message <- paste0("Invalid number format in units specification; ", w$message)
          } 
          stop(simpleError(w$message, w$call))
        } ),
      stringsAsFactors=FALSE))
  }) 
  unit_dfs
}

#' Wrap delimiter in regexp escape characters if appropriate
#' 
#' Helper to parse_units; defined outside parse_units for efficiency
prepare_delimiter <- function(delim) { 
  if(is.na(delim) | nchar(delim) != 1) {
    stop("delimiter must be exactly 1 character long")
  }
  if(delim %in% c('|', '.', '\\', '|', '(', ')', '[', '{', '^', '$', '*', '+', '?')) {
    return(paste0("\\",delim))
  } else {
    return(delim)
  }
}

#' Takes a units data.frame and returns the units in their character string representation.
#' 
#' Not exported.
#' 
#' @param unitdfs A list of unit data.frames, such as those returned by a call to parse_units
#' @return A list of character strings, one for each data.frame passed in
#' @family helpers
merge_units <- function(unitdfs) {
  require(plyr)
  strunits <- lapply(unitdfs, function(unitdf) {
    unitvec <- daply(unitdf, .(rownames(unitdf)), function(df){
      if(is.na(df$Power[1])) {
        return(paste0(df$Unit,"^NA"))
      } else if(df$Power[1]!=1) {
        return(paste(df$Unit,df$Power,sep="^"))
      } else {
        return(df$Unit)      
      }
    })
    paste(unitvec, collapse=" ")
  })
  return(unlist(strunits))
}

## simplify_units eliminates redundant units in the numerator and denominator.
simplify_units <- function(unitdfs) {
  unitdfs2 <- lapply(unitdfs, function(unitdf) {
    for (us in unique(unitdf$Unit)) {
      usrow <- which(unitdf$Unit == us)
      unitdf[usrow[1],"Power"] <- sum(unitdf[usrow,"Power"])
      # the following row deletions could be done all at once, perhaps, for efficiency.
      if(length(usrow)>1) unitdf <- unitdf[-usrow[2:length(usrow)],] 
      if(isTRUE(all.equal(unitdf[usrow[1],"Power"],0))) unitdf <- unitdf[-usrow[1],]
    }
    unitdf
  })
  return(unitdfs2)
}

## sort_units sorts the units data.frame by numerator/denominator and then alphabetically by the unit name.
#' @param unitdfs a list of unit data.frames (having columns Unit and Power)
#' @return a list of unit data.frames, each one sorted by num/denom and by unit name
sort_units <- function(unitdfs) {
  unitdfs2 <- lapply(unitdfs, function(unitdf) {
    newunitdf <- unitdf[order(unitdf$Power < 0, unitdf$Unit),]
    row.names(newunitdf) <- NULL 
    newunitdf
  })
  return(unitdfs2)
}
