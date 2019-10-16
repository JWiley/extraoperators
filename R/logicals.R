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
#' c(1, 3, 9, 5, NA, -9) %c% "is.na & (( >= 1 & <= 10 ) | == -9)"
#'
#' ## clean up
#' rm(sample_data)
`%c%`  <- function(e1, e2) {
  ## tests that input is correct
  stopifnot(is.character(e2))
  stopifnot(identical(length(e2), 1L))
  stopifnot(nzchar(e2))

  ## copy the call, used to find the
  ## symbol for e1
  call <- match.call()
  x <- as.character(call[2])

  ## replace | and & symbols temporarily
  e2 <- gsub(
    "\\|\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)",
    sprintf("__TEMP__OR__1 \\1(%s)", x),
    e2)

  e2 <- gsub(
    "&\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)",
    sprintf("__TEMP__AND__1 \\1(%s)", x),
    e2)

  e2 <- gsub("(\\||\\|\\s*\\(+|&|&\\s*\\(+)", sprintf("\\1 %s", x), e2)

  ## replace temp symbols back to R operators
  e2 <- gsub("__TEMP__OR__1",  "|", e2)
  e2 <- gsub("__TEMP__AND__1", "&", e2)

  ## deal with is.na or is.nan if it STARTS the operators
  if (grepl("^\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)", e2)) {
    e2 <- gsub("^\\s*(\\(*\\s*is.na|\\(*\\s*!is.na|\\(*\\s*is.nan|\\(*\\s*!is.nan)", sprintf("\\1(%s)", x), e2)
  } else {
    e2 <- gsub("^(\\s*\\(*)", sprintf("\\1 %s", x), e2)
  }

  ## find the parent frame and evalaute there
  e <- parent.frame()
  eval(parse(text = e2), envir = e)
}

#' Process Sets
#'
#' This is an internal function and not intended to be used directly.
#' It processes small sets.
#'
#' @param x A character string
#' @param envir An environment in which to evaluate values
#' @return A data frame with the processed set
#' @keywords internal
#' @examples
#'
#' ## ## below is an example that should generate an (informative) error
#' ## extraoperators:::.set1("(-Inf,x)", envir = environment())
#'
#' z <- max(mtcars$mpg)
#' extraoperators:::.set1("(-Inf,z)", envir = environment())
#'
#' extraoperators:::.set1("(-Inf,30)", envir = environment())
#'
#' ## clean up
#' rm(z)
.set1 <- function(x, envir) {
  if (identical(length(x), 1L) && identical(nchar(x), 1L)) { ## for connecting operators
    data.frame(Op1 = "",Val1 = "",
               Con1 = x,
               Op2 = "", Val2 = "",
               stringsAsFactors = FALSE)
  } else { ## for a set
    x <- unlist(strsplit(x, ","))
    if (isFALSE(grepl("\\(|\\[", x[1]) & grepl("\\)|\\]", x[2]))) {
      stop(paste(
        "Malformed inner set, requires both an open",
        "(i.e., '(' or '[') and a close (i.e., ')' or ']')",
        "brackets."))
    }
    if (missing(envir)) {
      envir <- parent.frame()
    }
    op1 <- ifelse(grepl("\\(", x[1]), ">", ">=")
    x[1] <- gsub("\\(|\\[", "", x[1])
    op2 <- ifelse(grepl("\\)", x[2]), "<", "<=")
    x[2] <- gsub("\\)|\\]", "", x[2])
    val1 <- tryCatch(
      as.character(eval(parse(text = x[1]), envir = envir)),
      error = function(e) "__AN-ERROR__")
    if (identical(val1, "__AN-ERROR__")) {
      stop(sprintf("Error, object '%s' not found", x[1]))
    }
    val2 <- tryCatch(
      as.character(eval(parse(text = x[2]), envir = envir)),
      error = function(e) "__AN-ERROR__")
    if (identical(val2, "__AN-ERROR__")) {
      stop(sprintf("Error, object '%s' not found", x[2]))
    }
    if (!identical(length(val1), 1L)) {
      stop(sprintf("evaluated text must be length 1, '%s' evaluates to length %d",
                   x[1], length(val1)))
    }
    if (!identical(length(val2), 1L)) {
      stop(sprintf("evaluated text must be length 1, '%s' evaluates to length %d",
                   x[2], length(val2)))
    }
    data.frame(Op1 = op1, Val1 = val1,
               Con1 = "&",
               Op2 = op2, Val2 = val2,
               stringsAsFactors = FALSE)
  }
}


#' Element In Set Operator
#'
#' This operator allows use of set notation style definitions
#'
#' @param e1 The values to be operated on, on the left hand side
#' @param e2 A character string containing set notation style
#' defined ranges on the real number line. Separate sets with the
#' \dQuote{&} or \dQuote{|} operator for AND or OR.
#' @keywords operators logical
#' @return a logical vector
#' @export
#' @examples
#'
#' c(-1, 0, 1, 9, 10, 16, 17, 20) %e% "(-Inf, 0) | [1, 9) | [10, 16] | (17, Inf]"
#' table(mtcars$mpg %e% "(0, 15.5) | [22.8, 40)")
#' table(mtcars$mpg %e% "(0, 15) | [16, 18] | [30, 50)")
#' c(-1, 0, 1) %e% "(-Inf, Inf) & [0, 0] | [1, 1]"
#'
#' z <- max(mtcars$mpg)
#' table(mtcars$mpg %e% "(-Inf, z)")
#'
#' ## clean up
#' rm(z)
`%e%` <- function(e1, e2) {
  call <- match.call()
  x <- as.character(call[2])

  envir <- parent.frame()

  e2 <- gsub("\\s", "", e2)
  e2 <- unlist(strsplit(e2, split = "&"))
  if (length(e2) > 1) {
    e2 <- c(unlist(lapply(e2[-length(e2)], c, "&")), e2[length(e2)])
  }
  e2 <- unlist(lapply(e2, function(x) {
    if (grepl("\\|", x)) {
      x <- unlist(strsplit(x, split = "\\|"))
      if (length(x) > 1) {
        x <- c(unlist(lapply(x[-length(x)], c, "|")), x[length(x)])
      }
    }
    return(x)
  }))
  e2 <- do.call(rbind, lapply(e2, .set1, envir = envir))
  e2$ConOnly <- nzchar(e2$Op1)

  test <- e2[e2$ConOnly == TRUE, , drop = FALSE]
  if (any(is.na(test$Val1) | is.nan(test$Val1) | is.na(test$Val2) | is.nan(test$Val2))) {
    stop("Values in sets cannot be missing")
  }
  if (!isTRUE(all(as.numeric(test$Val1) <= as.numeric(test$Val2)))) {
    stop("Values in sets must be ordered from least to greatest")
  }

  text <- paste(ifelse(nzchar(e2$Op1),
                       paste("(", x, e2$Op1, e2$Val1, e2$Con1, x, e2$Op2, e2$Val2, ")"),
                       e2$Con1), collapse = " ")
  eval(parse(text = text), envir = envir)
}
