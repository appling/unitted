#### tibble ####

#' Convert a unitted_list to a unitted_tibble
#' 
#' Implements \code{\link[tibble]{as_tibble}} for unitted_lists
#' 
#' @import tibble
#' @param x the unitted object to convert to a unitted_tbl_df
#' @export
#' @rdname as_tibble
#' @examples
#' tibble::as_tibble(
#'  u(list(x=u(1:3,"X"), y=u(3:5,"Y"), z=u(c("aa", "bb", "cc"),"Z"))))
as_tibble.unitted_list <- function(x) {
  u(as_tibble(v(x, partial=TRUE)))
}

#' Convert a unitted_data.frame to a unitted_tibble
#' 
#' Implements \code{\link[tibble]{as_tibble}} for unitted_data.frames
#' 
#' @import tibble
#' @export
#' @rdname as_tibble
#' @examples
#' tibble::as_tibble(
#'  u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z")))
as_tibble.unitted_data.frame <- function(x) {
  u(as_tibble(v(x, partial=TRUE)))
}

#' Convert a unitted_list to a unitted_tibble
#' 
#' Implements \code{\link[tibble]{as_tibble}} for unitted_tbl_dfs
#' 
#' @import tibble
#' @export
#' @rdname as_tibble
#' @examples
#' tibble::as_tibble(
#'  u(data.frame(x=u(1:3,"X"), y=u(3:5,"Y"), z=u(c("aa", "bb", "cc"),"Z"))))
as_tibble.unitted_tbl_df <- function(x) {
  x
}
