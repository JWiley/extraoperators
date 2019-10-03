#' @name logicals
#' @rdname logicals
#'
#' @title Several logical range comparison helpers
#'
#' @param e1 A number of vector to be evaluated
#' @param e2 A vector of one or two numbers used to denote the
#'   limits for logical comparison.
#'
#' @return A logical vector of the same length as \code{e1}.
NULL

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gele% c(2, 4)
#' 1:5 %gele% c(4, 2) # order does not matter uses min / max
`%gele%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 >= min(e2) & e1 <= max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gel% c(2, 4)
#' 1:5 %gel% c(4, 2) # order does not matter uses min / max
`%gel%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 >= min(e2) & e1 < max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gle% c(2, 4)
#' 1:5 %gle% c(4, 2) # order does not matter uses min / max
`%gle%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 > min(e2) & e1 <= max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gl% c(2, 4)
#' 1:5 %gl% c(4, 2) # order does not matter uses min / max
`%gl%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 > min(e2) & e1 < max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %g% c(2)
`%g%` <- function(e1, e2) {e1 > e2}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %ge% c(2)
`%ge%` <- function(e1, e2) {e1 >= e2}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %l% c(2)
`%l%` <- function(e1, e2) {e1 < e2}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %le% c(2)
`%le%` <- function(e1, e2) {e1 <= e2}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %!in% c(2, 99)
#' c("jack", "jill", "john", "jane") %!in% c("jill", "jane", "bill")
`%!in%` <- function(e1, e2) {
  stopifnot(length(e2) > 0)
  !(e1 %in% e2)
}

#' @rdname logicals
#' @export
`%nin%` <- `%!in%`

#' @rdname logicals
#' @export
## compare two strings where flips around
## a colon do not matter, used for interactions
`%flipIn%` <- function(e1, e2) {
  .flipMatch <- function(e1, e2) {
    e1 <- unlist(strsplit(e1, ":"))
    e2 <- unlist(strsplit(e2, ":"))
    all(e1 %in% e2) && all(e2 %in% e1)
  }
  sapply(e1, function(v) {
    .flipMatch(v, e2)
  })
}
