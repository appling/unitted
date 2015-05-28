#' Write a unitted object to file.
#' 
#' @importFrom dplyr %>% do
#' @exportMethod write_unitted
#' @param x The unitted object to be written. Currently only implemented for 
#'   data.frames.
#' @param file, The file to write to, as in \code{\link{write.table}}
#' @return NULL
#' @examples
#' write_unitted(u(data.frame(x=1:3, y=9:7), c(x="grapes","seeds")))
#' write_unitted(u(data.frame(x=1:3, y=9:7), c(x="mg L^-1","ft^3 s^-1")), sep="\t", file="practice.tsv")
#' file.remove("practice.tsv")
setGeneric(
  "write_unitted", 
  function(x, file="", ...) {
    standardGeneric("write_unitted")
  }
)

#' Write a unitted_data.frame to file.
#' 
#' @rdname write_unitted
#' @param comment.char a single character, or "", with which to prepend the line
#'   containing units information
#' @param sep the character string used to separate columns, as in 
#'   \code{\link{write.table}}
#' @param row.names logical. whether to write row names to the file, as in 
#'   \code{\link{write.table}}
#' @param quote logical. whether to place quotes around every data entry, as in 
#'   \code{\link{write.table}}
#' @param ... other arguments passed to \code{\link{write.table}}
setMethod(
  "write_unitted", "unitted_data.frame",
  function(x, file="", comment.char="#", sep="\t", row.names=FALSE, quote=FALSE, ...) {
    # prepare & check the dots arguments
    dots <- c(list(x=x, file=file, sep=sep, row.names=row.names, quote=quote), list(...))
    find_dot <- function(dotname) {
      if(!(dotname %in% names(dots))) formals(write.table)[[dotname]] else (dots[[dotname]])
    }
    
    # file logic copied from write.table
    file <- find_dot("file")
    if (file == "") 
      file <- stdout()
    else if (is.character(file)) {
      fileEncoding <- find_dot("fileEncoding")
      append <- find_dot("append")
      file <- if (nzchar(fileEncoding)) 
        file(file, ifelse(append, "a", "w"), encoding = fileEncoding)
      else file(file, ifelse(append, "a", "w"))
      on.exit(close(file))
    }
    else if (!isOpen(file, "w")) {
      open(file, "w")
      on.exit(close(file))
    }
    dots$file <- file
    
    # write the header. only write units if we're also writing the column names
    col.names <- find_dot("col.names")
    if(col.names) {
      # get the units information in data.frame format
      header <- x %>% get_units %>% as.list %>% as.data.frame(stringsAsFactors=FALSE, row.names="U")
      # write the column names line
      dots.colnames <- dots
      dots.colnames$x <- header[NULL,]
      do.call(write.table, dots.colnames)
      # write the units line, prepending a comment character
      dots.units <- dots
      dots.units$x <- header
      dots.units$col.names <- FALSE
      dots.units$quote <- TRUE
      dots.units$qmethod <- "escape"
      dots.units$file <- tempfile()
      do.call(write.table, dots.units)
      writeLines(text=paste0(comment.char, readLines(con=dots.units$file)), con=file, sep=find_dot("eol"))
    }
    
    # write the data, overriding append and col.names because we're always
    # appending without col.names at this point
    dots.data <- dots
    dots.data$x <- v(x)
    dots.data$append <- TRUE
    dots.data$col.names <- FALSE
    do.call(write.table, dots.data)
  }
)

#' Read a unitted data.frame from a text file that was written using 
#' write_unitted.
#' 
#' @param file The file to read in
#' @param comment.char a single-character string. should describe any comments 
#'   INCLUDING the special character, if any, that begins the units line (i.e., 
#'   the comment.char that was specified in write_unitted)
#' @param header logical indicating whether to read in the header (and units), 
#'   as in \code{\link{read.table}}
#' @param sep character string separating columns of the table, as in 
#'   \code{\link{read.table}}
#' @param stringsAsFactors logical indicating whether to convert strings to
#'   factors, as in \code{\link{read.table}}
#' @param ... Other arguments passed to \code{\link{read.table}}.
#' @param attach.units logical. If TRUE, the returned value is unitted. If 
#'   FALSE, units in the text file are ignored entirely and the return value is 
#'   a non-unitted data.frame.
#' @return a unitted_data.frame if attach.units=TRUE, or a data.frame if 
#'   attach.units=FALSE.
#' @export
#' @examples
#' practice <- u(data.frame(x=1:3, y=9:7, row.names=as.character(1:3)), c(x="grapes","seeds"))
#' write_unitted(practice, file="practice.tsv")
#' read_unitted("practice.tsv")
#' all.equal(practice, read_unitted("practice.tsv"))
#' file.remove("practice.tsv")
read_unitted <- function(file, comment.char="#", header=TRUE, sep="\t", stringsAsFactors=FALSE, ..., attach.units=TRUE) {
  # prepare & check the dots arguments
  dots <- c(list(file=file, comment.char=comment.char, header=header, sep=sep, stringsAsFactors=stringsAsFactors), list(...))
  find_dot <- function(dotname) {
    if(!(dotname %in% names(dots))) formals(read.table)[[dotname]] else (dots[[dotname]])
  }
  
  # read the data
  dat.v <- do.call(read.table, dots)
  
  # read the units
  if(isTRUE(attach.units) & isTRUE(find_dot("header"))) {
    # read the relevant rows
    num.units.rows <- 2
    total.rows <- find_dot("skip") + num.units.rows
    units.lines <- readLines(con=file, n=total.rows, encoding=find_dot("encoding"), skipNul=find_dot("skipNul"))[(total.rows+1-num.units.rows):total.rows]
    # remove the comment character from the beginning of the units line
    comment.char <- find_dot("comment.char")
    if(nchar(comment.char)>0) {
      units.line <- units.lines[length(units.lines)]
      if(comment.char != substr(units.line, 1, 1)) stop("expecting units line to start with this comment.char: ", comment.char)
      units.lines[length(units.lines)] <- substring(units.line, 2)
    }
    # read the colname and units lines as if they were a tiny data.frame
    dots.units <- dots
    dots.units$text <- paste0(units.lines, collapse="\n")
    dots.units$file <- NULL
    dots.units$colClasses <- "character"
    dots.units$quote <- "\"'"
    dots.units$stringsAsFactors <- FALSE
    dat.u <- do.call(read.table, dots.units)[1,,drop=TRUE] %>% unlist
  } else {
    # if there's no header, there should be no units
    dat.u <- rep(NA, ncol(dat.v))
  } 
    
  # combine the units & data
  return(if(isTRUE(attach.units)) u(dat.v, dat.u) else dat.v)
}