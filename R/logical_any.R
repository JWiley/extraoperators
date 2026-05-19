#' @name logical any
#' @rdname logicalany
#'
#' @title Several ways to evaluate whether any values meet logical conditions
#'
#' @param e1 A vector to be evaluated.
#' @param e2 The right hand side value passed to the underlying logical
#'   operator. See \code{\link{logicals}}, \code{\link{\%c\%}}, and
#'   \code{\link{\%e\%}} for operator-specific requirements.
#'
#' @return A logical value whether any \code{e1} meet the logical conditions.
NULL

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anygele% c(2, 4)
#' 1:5 %anygele% c(6, 7)
`%anygele%` <- function(e1, e2) {
  any(e1 %gele% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anygel% c(2, 4)
#' 1:5 %anygel% c(6, 7)
`%anygel%` <- function(e1, e2) {
  any(e1 %gel% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anygle% c(2, 4)
#' 1:5 %anygle% c(5, 6)
`%anygle%` <- function(e1, e2) {
  any(e1 %gle% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anygl% c(2, 4)
#' 1:5 %anygl% c(0, 1)
`%anygl%` <- function(e1, e2) {
  any(e1 %gl% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anyge% 2
#' 1:5 %anyge% 6
`%anyge%` <- function(e1, e2) {
  any(e1 %ge% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anyg% 2
#' 1:5 %anyg% 5
`%anyg%` <- function(e1, e2) {
  any(e1 %g% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anyle% 2
#' 1:5 %anyle% 0
`%anyle%` <- function(e1, e2) {
  any(e1 %le% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anyl% 2
#' 1:5 %anyl% 1
`%anyl%` <- function(e1, e2) {
  any(e1 %l% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %anyin% c(2, 99)
#' c("jack", "jill", "john", "jane") %anyin% c("jill", "bill")
`%anyin%` <- function(e1, e2) {
  any(e1 %in% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %any!in% c(2, 99)
#' c("jack", "jill", "john", "jane") %any!in% c("jill", "bill")
`%any!in%` <- function(e1, e2) {
  any(e1 %!in% e2)
}

#' @rdname logicalany
#' @export
`%anynin%` <- `%any!in%`

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %any==% c(5, 4, 3, 2, 1)
#' 1:5 %any==% 6:10
`%any==%` <- function(e1, e2) {
  any(e1 == e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' 1:5 %any!=% 1:5
#' 1:5 %any!=% c(5, 4, 3, 2, 1)
`%any!=%` <- function(e1, e2) {
  any(e1 != e2)
}

#' @rdname logicalany
#' @export
#' @examples
#' ## define a variable
#' sample_data <- c(1, 3, 9, 5, NA, -9)
#'
#' sample_data %anyc% "( >= 1 & <= 10 ) | == -9"
#' sample_data %anyc% "is.na"
#'
#' ## clean up
#' rm(sample_data)
`%anyc%`  <- function(e1, e2) {
  any(e1 %c% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#' ## define a variable
#' sample_data <- c(1, 3, 9, 5, -9)
#'
#' sample_data %anye% "(-8, 1] | [2, 9)"
#'
#' ## clean up
#' rm(sample_data)
`%anye%`  <- function(e1, e2) {
  any(e1 %e% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' c("jack", "jill", "john", "jane") %anygrepl% "^ja"
#' c("jack", "jill", "john", "jane") %anygrepl% "zz"
`%anygrepl%` <- function(e1, e2) {
  any(e1 %grepl% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' c("jack", "jill", "john", "jane") %any!grepl% "^ja"
#' c("jack", "jill", "john", "jane") %any!grepl% "j"
`%any!grepl%` <- function(e1, e2) {
  any(e1 %!grepl% e2)
}

#' @rdname logicalany
#' @export
#' @examples
#'
#' c("a:b", "c:d") %anyflipIn% "b:a"
#' c("a:b", "c:d") %anyflipIn% "x:y"
`%anyflipIn%` <- function(e1, e2) {
  any(e1 %flipIn% e2)
}
