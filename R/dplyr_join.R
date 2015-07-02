#' Join two unitted data.frames
#' 
#' Uses the corresponding dplyr join functions.
#' 
#' @name join
#' @inheritParams dplyr::inner_join
#' @importFrom dplyr inner_join
#' @export
inner_join.unitted_data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  unitted_join.unitted_data.frame(x, y, join_fun=inner_join, by=by, copy=copy, ...)
}

#' @rdname join
#' @importFrom dplyr left_join
#' @export
left_join.unitted_data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  unitted_join.unitted_data.frame(x, y, join_fun=left_join, by=by, copy=copy, ...)
}

#' @rdname join
#' @importFrom dplyr right_join
#' @export
right_join.unitted_data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  unitted_join.unitted_data.frame(x, y, join_fun=right_join, by=by, copy=copy, ...)
}

#' @rdname join
#' @importFrom dplyr full_join
#' @export
full_join.unitted_data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  unitted_join.unitted_data.frame(x, y, join_fun=full_join, by=by, copy=copy, ...)
}

#' @rdname join
#' @importFrom dplyr semi_join
#' @export
semi_join.unitted_data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  unitted_half_join.unitted_data.frame(x, y, join_fun=semi_join, by=by, copy=copy, ...)
}

#' @rdname join
#' @importFrom dplyr anti_join
#' @export
anti_join.unitted_data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  unitted_half_join.unitted_data.frame(x, y, join_fun=anti_join, by=by, copy=copy, ...)
}


#' Internal function to join two unitted data.frames
#' 
#' Appropriate to full, inner, left, and right joins, where the units are those
#' resulting from a separate join on the units alone
#' 
#' @keywords internal
unitted_join.unitted_data.frame <- function(ux, uy, join_fun=inner_join, ...) {
  # join the data
  vjoin <- join_fun(v(ux), v(uy), ...)
  # join the units, suppressing messages because they're probably duplicates of
  # those from the previous join_fun call
  ujoin <- suppressMessages(join_fun(
    as.data.frame(as.list(get_units(ux)), stringsAsFactors=FALSE), 
    as.data.frame(as.list(get_units(uy)), stringsAsFactors=FALSE), 
    ...))
  if(nrow(ujoin) != 1) {
    stop("Cannot join: Units mismatch in one or more data.frame columns")
  }
  u(vjoin, as.character(unlist(ujoin)))
}

#' Internal function to join two unitted data.frames
#' 
#' Appropriate to semi_join and anti_join, both of which retain the columns and
#' units of the first data.frame
#' 
#' @keywords internal
unitted_half_join.unitted_data.frame <- function(ux, uy, join_fun=inner_join, ...) {
  # join the data
  vjoin <- join_fun(v(ux), v(uy), ...)
  # assign the units, which will be the same as those of the original ux
  u(vjoin, get_unitbundles(ux))
}
