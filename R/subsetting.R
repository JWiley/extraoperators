#' @name subsetting
#' @rdname subsetting
#'
#' @title Several ways to subset based on logical range comparison helpers
#'
#' @param e1 A number of vector to be evaluated and subset
#' @param e2 A vector of one or two numbers used to denote the
#'   limits for logical comparison.
#'
#' @return A subset of \code{e1} that meets the logical conditions.
NULL

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sgele% c(2, 4)
#' 1:5 %sgele% c(4, 2) # order does not matter uses min / max
`%sgele%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1[e1 >= min(e2) & e1 <= max(e2)]
}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sgel% c(2, 4)
#' 1:5 %sgel% c(4, 2) # order does not matter uses min / max
`%sgel%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1[e1 >= min(e2) & e1 < max(e2)]
}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sgle% c(2, 4)
#' 1:5 %sgle% c(4, 2) # order does not matter uses min / max
`%sgle%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1[e1 > min(e2) & e1 <= max(e2)]
}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sgl% c(2, 4)
#' 1:5 %sgl% c(4, 2) # order does not matter uses min / max
`%sgl%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))
  e1[e1 > min(e2) & e1 < max(e2)]
}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sge% 2
#' 1:5 %sge% 4
`%sge%` <- function(e1, e2) {e1[e1 >= e2]}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sg% 2
#' 1:5 %sg% 4
`%sg%` <- function(e1, e2) {e1[e1 > e2]}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sle% 2
#' 1:5 %sle% 4
`%sle%` <- function(e1, e2) {e1[e1 <= e2]}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sl% 2
#' 1:5 %sl% 4
`%sl%` <- function(e1, e2) {e1[e1 < e2]}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %sin% c(2, 99)
#' c("jack", "jill", "john", "jane") %sin% c("jill", "jane", "bill")
`%sin%` <- function(e1, e2) {e1[e1 %in% e2]}

#' @rdname subsetting
#' @export
#' @examples
#'
#' 1:5 %s!in% c(2, 99)
#' c("jack", "jill", "john", "jane") %s!in% c("jill", "jane", "bill")
`%s!in%` <- function(e1, e2) {e1[e1 %!in% e2]}

#' @rdname subsetting
#' @export
`%snin%` <- `%s!in%`
