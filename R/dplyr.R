#### select, rename, mutate, ... ####

#' Select/rename variables by name for unitted data.frames & tibbles
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
#' dplyr::select(
#'  u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z")), 
#'  a=y, x)
select.unitted_data.frame <- function (.data, ...) {
  # copy lines from dplyr:::select.data.frame
  vars <- dplyr::select_vars(names(.data), !!! quos(...))
  # call dplyr::select rather than dplyr:::select_impl to preserve :::
  dplyr::select(v(.data), ...) %>%
    u(get_unitbundles(.data)[vars])
}

#' Implements dplyr::select and dplyr::select_ for unitted_tbl_dfs
#' 
#' @importFrom dplyr select_ select_vars_
#' @importFrom lazyeval all_dots
#' @export
#' 
#' @rdname select
#' @examples
#' dplyr::select(
#'  tibble::as_tibble(u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z"))), 
#'  a=y, x)
select.unitted_tbl_df <- select.unitted_data.frame


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
rename.unitted_data.frame <- function (.data, ...) {
  vars <- rename_vars(names(.data), !!! quos(...))
  # call dplyr::select rather than dplyr:::select_impl to preserve :::
  dplyr::rename(v(.data), ...) %>%
    u(get_unitbundles(.data)[vars])
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
#' dplyr::rename(tibble::as_tibble(df), a=y, beta=x)
rename.unitted_tbl_df <- rename.unitted_data.frame


#' Implements dplyr::mutate and dplyr::mutate_ for unitted_data.frames
#' 
#' @param .data a unitted_data.frame
#' @param ... standard dots, as in \code{\link[dplyr]{select_}}
#' @param .dots nonstandard dots, as in \code{\link[dplyr]{select_}}
#' @return a unitted_data.frame after the \code{\link[dplyr]{select_}} operation
#' 
#' @importFrom dplyr tibble mutate_
#' @importFrom lazyeval all_dots
#' @export
#'
#' @examples
#' df <- u(data.frame(x=1:3, y=3:5), c("X","Y"))
#' dplyr::mutate(df, z=LETTERS[y], k=x*y)
mutate_.unitted_data.frame <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  as.data.frame(dplyr::mutate_(as_tibble(.data), .dots=dots))
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
#' dtb <- tibble::as_tibble(u(data.frame(x=1:3, y=3:5), c("X","X")))
#' dplyr::mutate(dtb, z=LETTERS[y], k=x+y)
mutate_.unitted_tbl_df <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  u(dplyr::mutate_(v(.data, partial=TRUE), .dots=dots))
}
