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

#' Chain Operator
#'
#' This operator allows operators on the right hand side to be
#' chained together. The intended use case is when you have a
#' single object on which you want to perform several operations.
#' For example, testing whether a variable is between 1 and 5 or
#' equals special number 9, which might be used to indicate that
#' someone responded to a question (i.e., its not missing per se)
#' but that they preferred not to answer or did not know the answer.
#'
#' `%c%` has special facilities to handle the following:
#' `is.na`, `!is.na`, `is.nan`, and `!is.nan`. These do not need
#' any values supplied but they work as expected to add those logical
#' assessments into the chain of operators.
#'
#' @param e1 The values to be operated on, on the left hand side
#' @param e2 A character string (it MUST be quoted) containing all
#' the operators and their values to apply to `e1`. Note that in
#' this character string, operators can be chained together using either
#' `|` or `&`. Parentheses are also supported and work as expected. See
#' examples for more information on how this function is used.
#' @keywords operators logical
#' @return a logical vector
#' @export
#' @examples
#'
#' ## define a variable
#' sample_data <- c(1, 3, 9, 5, NA, -9)
#'
#' ## suppose that we expect that values should fall in [1, 10]
#' ## unless they are special character, -9 used for unknown / refused
#' sample_data %c% "( >= 1 & <= 10 ) | == -9"
#'
#' ## we might expect some missing values and be OK as long as
#' ## above conditions are met or values are missing
#' sample_data %c% "( >= 1 & <= 10 ) | == -9 | is.na"
#'
#' ## equally we might be expecting NO missing values
#' ## and want missing values to come up as FALSE
#' sample_data %c% "(( >= 1 & <= 10 ) | == -9) & !is.na"
#'
#' ## clean up
#' rm(sample_data)
`%c%`  <- function(e1, e2) {
  stopifnot(is.character(e2))
  stopifnot(identical(length(e2), 1L))
  stopifnot(nzchar(e2))

  call <- match.call()
  x <- as.character(call[2])

  ## browser()
  e2 <- gsub(
    "\\|\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)",
    sprintf("__TEMP__OR__1 \\1(%s)", x),
    e2)

  e2 <- gsub(
    "&\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)",
    sprintf("__TEMP__AND__1 \\1(%s)", x),
    e2)

  e2 <- gsub("(\\||\\|\\s*\\(|&|&\\s*\\()", sprintf("\\1 %s", x), e2)

  e2 <- gsub("__TEMP__OR__1",  "|", e2)
  e2 <- gsub("__TEMP__AND__1", "&", e2)

  if (grepl("^\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)", e2)) {
    e2 <- gsub("^\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)", sprintf("\\1(%s)", x), e2)
  } else {
    e2 <- gsub("^(\\s*\\(*)", sprintf("\\1 %s", x), e2)
  }

  e <- parent.frame()
  eval(parse(text = e2), envir = e)
}

