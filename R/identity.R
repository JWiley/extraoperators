#' Strict Identity Operators
#'
#' These operators are wrappers around \code{identical()} for whole-object
#' identity checks. They return a single logical value and are not elementwise
#' comparisons.
#'
#' @name identity_operators
#' @param e1 An object to compare.
#' @param e2 An object to compare.
#' @return A single logical value.
NULL

#' @rdname identity_operators
#' @export
#' @examples
#'
#' 1:3 %===% 1:3
#' 1 %===% 1L
`%===%` <- function(e1, e2) {
  identical(e1, e2)
}

#' @rdname identity_operators
#' @export
#' @examples
#'
#' 1:3 %!==% 3:1
#' 1 %!==% 1L
`%!==%` <- function(e1, e2) {
  !identical(e1, e2)
}
