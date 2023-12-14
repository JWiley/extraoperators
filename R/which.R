#' @name logical indexes (which)
#' @rdname logicalwhich
#'
#' @title Several ways to return an index based on logical range comparison helpers
#'
#' @param e1 A number of vector to be evaluated and for which the indices will be returned
#' @param e2 A vector of one or two numbers used to denote the
#'   limits for logical comparison.
#'
#' @return A vector of the indices identifying which values of \code{e1} meet the logical conditions.
NULL

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?gele% c(2, 4)
#' 1:5 %?gele% c(4, 2) # order does not matter uses min / max
`%?gele%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  which(e1 >= min(e2) & e1 <= max(e2))
}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?gel% c(2, 4)
#' 1:5 %?gel% c(4, 2) # order does not matter uses min / max
`%?gel%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  which(e1 >= min(e2) & e1 < max(e2))
}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?gle% c(2, 4)
#' 1:5 %?gle% c(4, 2) # order does not matter uses min / max
`%?gle%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  which(e1 > min(e2) & e1 <= max(e2))
}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?gl% c(2, 4)
#' 1:5 %?gl% c(4, 2) # order does not matter uses min / max
`%?gl%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))
  which(e1 > min(e2) & e1 < max(e2))
}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?ge% 2
#' 1:5 %?ge% 4
`%?ge%` <- function(e1, e2) {which(e1 >= e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?g% 2
#' 1:5 %?g% 4
`%?g%` <- function(e1, e2) {which(e1 > e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?le% 2
#' 1:5 %?le% 4
`%?le%` <- function(e1, e2) {which(e1 <= e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?l% 2
#' 1:5 %?l% 4
`%?l%` <- function(e1, e2) {which(e1 < e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?in% c(2, 99)
#' c("jack", "jill", "john", "jane") %?in% c("jill", "jane", "bill")
`%?in%` <- function(e1, e2) {which(e1 %in% e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?!in% c(2, 99)
#' c("jack", "jill", "john", "jane") %?!in% c("jill", "jane", "bill")
`%?!in%` <- function(e1, e2) {which(e1 %!in% e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 1:5 %?nin% c(2, 99)
#' c("jack", "jill", "john", "jane") %snin% c("jill", "jane", "bill")
`%?nin%` <- function(e1, e2) {which(e1 %!in% e2)}


#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 11:15 %?==% c(11, 1, 13, 15, 15)
`%?==%` <- function(e1, e2) {which(e1 == e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' 11:15 %?!=% c(11, 1, 13, 15, 15)
`%?!=%` <- function(e1, e2) {which(e1 != e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#' ## define a variable
#' sample_data <- c(1, 3, 9, 5, NA, -9)
#'
#' ## suppose that we expect that values should fall in [1, 10]
#' ## unless they are special character, -9 used for unknown / refused
#' sample_data %?c% "( >= 1 & <= 10 ) | == -9"
#'
#' ## we might expect some missing values and be OK as long as
#' ## above conditions are met or values are missing
#' sample_data %?c% "( >= 1 & <= 10 ) | == -9 | is.na"
#'
#' ## equally we might be expecting NO missing values
#' ## and want missing values to come up as FALSE
#' sample_data %?c% "(( >= 1 & <= 10 ) | == -9) & !is.na"
#'
#' ## clean up
#' rm(sample_data)
`%?c%`  <- function(e1, e2) {
  which(e1 %c% e2)
}


#' @rdname logicalwhich
#' @export
#' @examples
#' ## define a variable
#' sample_data <- c(1, 3, 9, 5, -9)
#'
#' sample_data %?e% "(-8, 1] | [2, 9)"
#'
#' ## clean up
#' rm(sample_data)
`%?e%`  <- function(e1, e2) {
  which(e1 %e% e2)
}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' c("jack", "jill", "john", "jane", "sill", "ajay") %?grepl% "ja"
#' c("jack", "jill", "john", "jane", "sill", "ajay") %?grepl% "^ja"
`%?grepl%` <- function(e1, e2) {which(e1 %grepl% e2)}

#' @rdname logicalwhich
#' @export
#' @examples
#'
#' c("jack", "jill", "john", "jane", "sill", "ajay") %?!grepl% "ja"
#' c("jack", "jill", "john", "jane", "sill", "ajay") %?!grepl% "^ja"
`%?!grepl%` <- function(e1, e2) {which(e1 %!grepl% e2)}