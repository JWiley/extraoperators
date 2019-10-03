#' @name logical all
#' @rdname logicalall
#'
#' @title Several ways to evaluate whether all values meet logical conditions including logical range comparison helpers
#'
#' @param e1 A number of vector to be evaluated
#' @param e2 A vector of one or two numbers used to denote the
#'   limits for logical comparison.
#'
#' @return A logical value whether all \code{e1} meet the logical conditions.
NULL

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %agele% c(2, 4)
#' 1:5 %agele% c(4, 2) # order does not matter uses min / max
`%agele%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  all(e1 >= min(e2) & e1 <= max(e2))
}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %agel% c(2, 4)
#' 1:5 %agel% c(4, 2) # order does not matter uses min / max
`%agel%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  all(e1 >= min(e2) & e1 < max(e2))
}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %agle% c(2, 4)
#' 1:5 %agle% c(4, 2) # order does not matter uses min / max
`%agle%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  all(e1 > min(e2) & e1 <= max(e2))
}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %agl% c(2, 4)
#' 1:5 %agl% c(4, 2) # order does not matter uses min / max
`%agl%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))
  all(e1 > min(e2) & e1 < max(e2))
}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %age% 2
#' 1:5 %age% 4
`%age%` <- function(e1, e2) {all(e1 >= e2)}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %ag% 2
#' 1:5 %ag% 4
`%ag%` <- function(e1, e2) {all(e1 > e2)}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %ale% 2
#' 1:5 %ale% 4
`%ale%` <- function(e1, e2) {all(e1 <= e2)}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %al% 2
#' 1:5 %al% 4
`%al%` <- function(e1, e2) {all(e1 < e2)}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %ain% c(2, 99)
#' c("jack", "jill", "john", "jane") %ain% c("jill", "jane", "bill")
`%ain%` <- function(e1, e2) {all(e1 %in% e2)}

#' @rdname logicalall
#' @export
#' @examples
#'
#' 1:5 %a!in% c(2, 99)
#' c("jack", "jill", "john", "jane") %a!in% c("jill", "jane", "bill")
`%a!in%` <- function(e1, e2) {all(e1 %!in% e2)}

#' @rdname logicalall
#' @export
`%anin%` <- `%a!in%`
