# Functions used by unitbundle.R for turning units into strings and vice versa

#' Turn a units string into an internal units representation
#' 
#' This function creates parsed units. Unlike unparsed character strings, parsed
#' units are unambiguous and can be used for arithmetic operations.
#' 
#' Units strings are potentially ambiguous; a delimiter may be used to indicate 
#' where the text part of a unit begins and ends. For example, "g soil^-1" will 
#' be incorrectly parsed into "g" and "soil^-1", but this can be avoided by 
#' wrapping the two-word unit in the delimiter: "|g soil|^-1" will be parsed 
#' such that "g soil" is a single unit term.
#' 
#' \subsection{Definitions}{ This is a unit: "kg". This is another unit: 
#' "kgC_organic^2". This is a unit string: "kgC ha^-1 yr^-1". This is a vector 
#' of unit strings: c("kgC ha^-1 yr^-1", "PgN", "ft^3 s^-1"). }
#' 
#' \subsection{Implementation}{ This function is vectorized so that N unit 
#' strings will be turned into N internal representations. }
#' 
#' @param ustrs character vector of unit strings, one string per set of units
#' @param delimiter single character. The delimiter can be used to identify 
#'   complex units - for example, with "|" as the delimiter, you might specify 
#'   units of "|g SO_4^2-| m^-2 hr^-1". Then "g SO_4^2-" would be considered a 
#'   single unit with power of 1. "m" and "hr" would also be recognized as units
#'   because they are separated by spaces and contain no internal special 
#'   characters (such as spaces or "^").
#' @return list of data.frames, one per unit string passed in
#' @family unit-parsing functions
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
#' Helper to parse_units
#' 
#' @param delim character. A delimiter string to be wrapped in escape characters for use in regular expressions.
#' @return The wrapped delimiter. 
#' @family unit-parsing functions
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


#' Combine redundant units and eliminate units raised to the 0th power
#' 
#' @param unitdfs list of unit data.frames to be simplified
#' @return list of simplified unit data.frames
#' @family unit-parsing functions
simplify_units <- function(unitdfs) {
  unitdfs2 <- lapply(unitdfs, function(unitdf) {
    for (us in unique(unitdf$Unit)) {
      usrow <- which(unitdf$Unit == us)
      unitdf[usrow[1],"Power"] <- sum(unitdf[usrow,"Power"])
      if(length(usrow)>1) unitdf <- unitdf[-usrow[2:length(usrow)],] 
      if(isTRUE(all.equal(unitdf[usrow[1],"Power"],0))) unitdf <- unitdf[-usrow[1],]
    }
    unitdf
  })
  return(unitdfs2)
}

#' Sort the units data.frame by numerator/denominator and then alphabetically by
#' the unit name
#' 
#' @param unitdfs list of unit data.frames (having columns Unit and Power)
#' @return list of unit data.frames, each one sorted by num/denom and by unit
#'   name
#' @family unit-parsing functions
sort_units <- function(unitdfs) {
  unitdfs2 <- lapply(unitdfs, function(unitdf) {
    newunitdf <- unitdf[order(unitdf$Power < 0, unitdf$Unit),]
    row.names(newunitdf) <- NULL 
    newunitdf
  })
  return(unitdfs2)
}


#' Turn a units data.frame into the character representation of the units
#' 
#' @param unitdfs list of unit data.frames, such as those returned by a call to 
#'   parse_units
#' @param delimiter A single-character string designating the delimiter that 
#'   should surround those units that are to be delimited according to 
#'   \code{rule}
#' @param rule character string indicating the rule by which each 
#'   unit within a unitdf will be delimited or not. The default, "disambiguate",
#'   wraps only those units that contain spaces or "^" characters. "never" wraps
#'   none, and "always" wraps all.
#' @return list of character strings, one for each data.frame passed in
#' @family unit-merging functions
merge_units <- function(unitdfs, delimiter="|", rule=c("disambiguate","never","always")) {
  # for rule, accept exactly one of those options listed in the function
  # definition, taking the first as the default. Convert to integer.
  rule <- match.arg(rule)
  require(plyr)
  # merge the units within each unitdf
  strunits <- lapply(unitdfs, function(unitdf) {
    unitdf$Unit <- delimit_units(unitdf$Unit, delimiter, rule)
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

#' Wrap a unit in delimiters if required
#' 
#' A helper to merge_units that wraps the unit in the delimiter, or not, 
#' according to \code{rule}.
#' 
#' @param units Character string[s] each containing a single unit
#' @param delimiter A single-character string indicating the delimiter to be 
#'   used
#' @param rule Character string indicating the delimitation rule to follow.
#' @return A character vector of the same length as \code{units}, with each
#'   string delimited if required by \code{rule}.
#' @family unit-merging functions
delimit_units <- function(units, delimiter, rule) {
  switch(
    rule,
    disambiguate={ 
      sapply(units, function(unit) {
        # Look for ambiguous characters - a space or caret - to decide whether to
        # delimit each string
        if(grepl("[[:space:]|^]", unit)) {
          paste0(delimiter, unit, delimiter)
        } else {
          unit
        }
      }) },
    never=units,
    always=paste0(delimiter, units, delimiter)
  )      
}
