test_that("logical comparisons work", {
  expect_true(1 %g% 0)
  expect_false(1 %g% 1)

  expect_true(1 %ge% 0)
  expect_true(1 %ge% 1)
  expect_false(1 %ge% 2)

  expect_false(1 %l% 0)
  expect_false(1 %l% 1)
  expect_true(1 %l% 2)

  expect_false(1 %le% 0)
  expect_true(1 %le% 1)
  expect_true(1 %le% 2)

  expect_true(1 %gele% c(1, 3))
  expect_false(1 %gele% c(2, 3))

  expect_true(1 %gle% c(0, 3))
  expect_false(1 %gle% c(1, 3))

  expect_true(1 %gel% c(0, 2))
  expect_false(1 %gel% c(0, 1))

  expect_true(1 %gl% c(0, 2))
  expect_false(1 %gl% c(1, 2))
  expect_false(1 %gl% c(0, 1))

  expect_true(1 %in% c(1, 2))
  expect_false(1 %in% c(3, 2))

  expect_true(1 %!in% c(3, 2))
  expect_false(1 %!in% c(1, 2))
  expect_true(1 %nin% c(3, 2))
  expect_false(1 %nin% c(1, 2))

  expect_equivalent(c("a:b", "c:d") %flipIn% "b:a", c(TRUE, FALSE))


  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "( >= 1 & <= 10 ) | == -9",
    c(TRUE, TRUE, TRUE, TRUE, NA, TRUE))

  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "( >= 1 & <= 10 ) | == -9 | is.na",
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "is.na | ( >= 1 & <= 10 ) | == -9",
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "((is.na)) | ( >= 1 & <= 10 ) | == -9",
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "(( >= 1 & <= 10 ) | == -9) & !is.na",
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))

  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "!is.na & (( >= 1 & <= 10 ) | == -9)",
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))

  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "(!is.na) & (( >= 1 & <= 10 ) | == -9)",
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))

  expect_equivalent(
    c(1, 3, 9, 5, NA, -9) %c% "(((!is.na))) & (( >= 1 & <= 10 ) | == -9)",
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))

  expect_equivalent(
    .set1("(-Inf,30)", envir = environment()),
    data.frame(Op1 = ">", Val1 = "-Inf", Con1 = "&",
               Op2 = "<", Val2 = "30",
               stringsAsFactors = FALSE))

  expect_equivalent(
    .set1("(-Inf,30)"),
    data.frame(Op1 = ">", Val1 = "-Inf", Con1 = "&",
               Op2 = "<", Val2 = "30",
               stringsAsFactors = FALSE))


  expect_equivalent(
    .set1("&", envir = environment()),
    data.frame(Op1 = "", Val1 = "", Con1 = "&",
               Op2 = "", Val2 = "",
               stringsAsFactors = FALSE))

  expect_true(0 %e% "(-Inf, 0]")
  expect_false(1 %e% "(-Inf, 0]")
  expect_false(any(c(0, 9, 17) %e% "(-Inf, 0) | (5, 9) | (17, 40)"))
  expect_false(any(c(0, 9, 17) %e% "(-Inf, Inf) & (5, 9)"))

})


test_that("logical operator error checking works", {
  expect_error(1 %gele% 1)
  expect_error(1 %gele% c(1, 3, 5))
  expect_error(1 %gele% c(1, NA_real_))

  expect_error(1 %gel% 1)
  expect_error(1 %gel% c(1, 3, 5))
  expect_error(1 %gel% c(1, NA_real_))

  expect_error(1 %gle% 1)
  expect_error(1 %gle% c(1, 3, 5))
  expect_error(1 %gle% c(1, NA_real_))

  expect_error(1 %gl% 1)
  expect_error(1 %gl% c(1, 3, 5))
  expect_error(1 %gl% c(1, NA_real_))

  expect_error(1 %!in% numeric(0))
  expect_error(1 %nin% numeric(0))

  expect_error(1 %c% c(" > 3", "< 1"))
  expect_error(1 %c% 1)
  expect_error(1 %c% "")

  expect_error(extraoperators:::.set1("(-Inf,x)", envir = environment()))
  expect_error(extraoperators:::.set1("(-Inf,30", envir = environment()))
  expect_error(extraoperators:::.set1("(x,30)", envir = environment()))
  expect_error(extraoperators:::.set1("(-Inf,mtcars$mpg)", envir = environment()))
  expect_error(extraoperators:::.set1("(mtcars$mpg, 50)", envir = environment()))

  expect_error(0 %e% "(15, 1)")
  expect_error(0 %e% "(15, NA)")
})

