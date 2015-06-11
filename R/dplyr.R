#### tbl_df ####

#' Implements dplyr::tbl_df
#' 
#' This is not generic in dplyr but is made so here.
#' 
#' @param data A list, as in \code{\link[dplyr]{as_data_frame}}. Each element of 
#'   the list must have the same length.
#' @examples
#' tbl_df(u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z")))
#' tbl_df(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")))
#' @export
tbl_df <- function(data) {
  if(!is.data.frame(data)) { stop("data must be a data.frame") }
  as_data_frame(data)
}

#' Implements dplyr::as_data_frame
#' 
#' dplyr's \code{\link[dplyr]{as_data_frame}} serves other dplyr functions 
#' including \code{\link[dplyr]{tbl_df}} and \code{\link[dplyr]{mutate}}.
#' 
#' This is not generic in dplyr but is made so here.
#' 
#' @param x A list, as in \code{\link[dplyr]{as_data_frame}}. Each element of 
#'   the list must have the same length.
#' @examples
#' as_data_frame(u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z")))
#' as_data_frame(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")))
#' @export
as_data_frame <- function(x) {
  UseMethod("as_data_frame")
}

#' Convert a data.frame or list to a tbl_df
#' 
#' @importFrom dplyr as_data_frame
#' @examples
#' as_data_frame(list(x = 1:500, y = runif(500), z = 500:1))
#' as_data_frame(data.frame(x = 1:500, y = runif(500), z = 500:1))
#' @export
#' @rdname as_data_frame
as_data_frame.default <- function(x) {
  dplyr::as_data_frame(x)
}


#' Convert a unitted_list to a unitted_tbl_df
#' 
#' Implements \code{\link[dplyr]{as_data_frame}} for unitted_data.frames
#' 
#' @examples
#' as_data_frame(u(list(x=u(1:3,"X"), y=u(3:5,"Y"), z=u(c("aa", "bb", "cc"),"Z"))))
#' @export
#' @rdname as_data_frame
as_data_frame.unitted_list <- function(x) {
  u(as_data_frame(v(x, partial=TRUE)))
}

#' Convert a unitted_data.frame to a unitted_tbl_df
#' 
#' Implements \code{\link[dplyr]{as_data_frame}} for unitted_data.frames
#' 
#' @examples
#' as_data_frame(u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z")))
#' @export
#' @rdname as_data_frame
as_data_frame.unitted_data.frame <- function(x) {
  u(as_data_frame(v(x, partial=TRUE)))
}


#### select, rename, mutate, ... ####

#' Column selection for unitted data.frames, tbl_dfs, etc
#' 
#' @param .data a unitted_data.frame
#' @param ... standard dots, as in \code{\link[dplyr]{rename_}}
#' @param .dots nonstandard dots, as in \code{\link[dplyr]{rename_}}
#' @return a unitted_data.frame after the \code{\link[dplyr]{rename_}} operation
#' 
#' @name select
NULL

#' Implements dplyr::select and dplyr::select_ for unitted_data.frames
#' 
#' @importFrom dplyr select_ select_vars_
#' @importFrom lazyeval all_dots
#' @export
#'
#' @rdname select
#' @examples
#' dplyr::select(u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z")), a=y, x)
select_.unitted_data.frame <- function (.data, ..., .dots) {
  # copy lines from dplyr:::select_.data.frame
  dots <- lazyeval::all_dots(.dots, ...)
  .select_unitted(.data, dots)
}

#' Implements dplyr::select and dplyr::select_ for unitted_tbl_dfs
#' 
#' @importFrom dplyr select_ select_vars_
#' @importFrom lazyeval all_dots
#' @export
#'
#' @rdname select
#' @examples
#' dplyr::select(tbl_df(u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z"))), a=y, x)
select_.unitted_tbl_df <- function (.data, ..., .dots) {
  # copy lines from dplyr:::select_.data.frame
  dots <- lazyeval::all_dots(.dots, ...)
  .select_unitted(.data, dots)
}

#' Internal function for selecting from unitted objects
.select_unitted <- function (.data, .dots) {
  
  # copy lines from dplyr:::select_.data.frame
  vars <- select_vars_(names(.data), .dots)
  
  # use dplyr's internal functions to select/rename the data columns
  select_(v(.data), .dots=vars) %>%
    # use the vars from select_vars_ above to select the units
    u(get_unitbundles(.data)[vars])
  
}



#' Implements dplyr::rename and dplyr::rename_ for unitted_data.frames
#' 
#' @importFrom dplyr select_ rename_vars_
#' @importFrom lazyeval all_dots
#' @export
#'
#' @rdname select
#' @examples
#' df <- u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z"))
#' dplyr::rename(df, a=y, beta=x)
rename_.unitted_data.frame <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  .rename_unitted(.data, .dots=dots)
}

#' Implements dplyr::rename and dplyr::rename_ for unitted_tbl_dfs
#' 
#' @return a unitted_data.frame after the \code{\link[dplyr]{rename_}} operation
#' 
#' @importFrom dplyr select_ rename_vars_
#' @importFrom lazyeval all_dots
#' @export
#'
#' @rdname select
#' @examples
#' df <- u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z"))
#' dplyr::rename(tbl_df(df), a=y, beta=x)
rename_.unitted_tbl_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  .rename_unitted(.data, .dots=dots)
}
  
#' Internal function for renaming unitted objects
.rename_unitted <- function (.data, .dots) {
  
  # copy lines from dplyr:::select_.data.frame
  vars <- rename_vars_(names(.data), .dots)
  
  # use dplyr's internal functions to select/rename the data columns
  select_(v(.data), .dots=vars) %>%
    # use the vars from select_vars_ above to select the units
    u(get_unitbundles(.data)[vars])
  
}

#' Implements dplyr::mutate and dplyr::mutate_ for unitted_data.frames
#' 
#' @param .data a unitted_data.frame
#' @param ... standard dots, as in \code{\link[dplyr]{select_}}
#' @param .dots nonstandard dots, as in \code{\link[dplyr]{select_}}
#' @return a unitted_data.frame after the \code{\link[dplyr]{select_}} operation
#' 
#' @importFrom dplyr tbl_df mutate_
#' @importFrom lazyeval all_dots
#' @export
#'
#' @examples
#' df <- u(data.frame(x=1:3, y=3:5), c("X","Y"))
#' dplyr::mutate(df, z=LETTERS[y], k=x*y)
mutate_.unitted_data.frame <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(dplyr::mutate_(tbl_df(.data), .dots=dots))
}

#' Implements dplyr::mutate and dplyr::mutate_ for unitted_tbl_dfs
#' 
#' @param .data a unitted_tbl_df
#' @param ... standard dots, as in \code{\link[dplyr]{select_}}
#' @param .dots nonstandard dots, as in \code{\link[dplyr]{select_}}
#' @return a unitted_tbl_df after the \code{\link[dplyr]{select_}} operation
#' 
#' @importFrom dplyr mutate_
#' @importFrom lazyeval all_dots
#' @export
#' @examples
#' dtb <- tbl_df(u(data.frame(x=1:3, y=3:5), c("X","X")))
#' dplyr::mutate(dtb, z=LETTERS[y], k=x+y)
mutate_.unitted_tbl_df <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  u(dplyr::mutate_(v(.data, partial=TRUE), .dots=dots))
}