#' Implements dplyr::select and dplyr::select_ for unitted_data.frames
#' 
#' @param .data a unitted_data.frame
#' @param ... standard dots, as in \code{\link[dplyr]{select_}}
#' @param .dots nonstandard dots, as in \code{\link[dplyr]{select_}}
#' @return a unitted_data.frame after the \code{\link[dplyr]{select_}} operation
#' 
#' @importFrom dplyr select_ select_vars_
#' @importFrom lazyeval all_dots
#' @export
#'
#' @examples
#' dplyr::select(u(data.frame(x=1:3, y=3:5, z=c("aa", "bb", "cc")), c("X","Y","Z")), a=y, x)
select_.unitted_data.frame <- function (.data, ..., .dots) {

  # copy lines from dplyr:::select_.data.frame
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots)
  
  # use dplyr's internal functions to select/rename the data columns
  select_(v(.data), .dots=vars) %>%
    # use the vars from select_vars_ above to select the units
    u(get_unitbundles(.data)[vars])
  
}
