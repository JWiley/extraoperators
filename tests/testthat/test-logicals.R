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
    c(1, 3, 9, 5, NA, -9) %c% "(( >= 1 & <= 10 ) | == -9) & !is.na",
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))

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
})


test_that("subsetting works", {
  expect_equal(1:5 %sg% 3, c(4, 5))

  expect_equal(1:5 %sge% 3, c(3, 4, 5))

  expect_equal(1:5 %sl% 3, c(1, 2))

  expect_equal(1:5 %sle% 3, c(1, 2, 3))

  expect_equal(1:5 %sgl% c(2, 4), c(3))

  expect_equal(1:5 %sgle% c(2, 4), c(3, 4))

  expect_equal(1:5 %sgele% c(2, 4), c(2, 3, 4))

  expect_equal(1:5 %sgel% c(2, 4), c(2, 3))

  expect_equal(1:5 %sin% c(2, 4, 6), c(2, 4))

  expect_equal(1:5 %s!in% c(2, 4, 6), c(1, 3, 5))
  expect_equal(1:5 %snin% c(2, 4, 6), c(1, 3, 5))

  expect_equal(1:5 %s==% 1:5, 1:5)
  expect_equal(1:5 %s==% c(1:4, 1), 1:4)

  expect_equal(1:5 %s!=% 1:5, integer(0))
  expect_equal(1:5 %s!=% c(1:4, 1), 5)

  expect_equal(
    c(1, 3, 9, 5, NA, -9) %sc% "( >= 1 & <= 10 ) | == -9",
    c(1, 3, 9, 5, NA, -9))
  expect_equal(
    c(1, 3, 9, 5, NA, -9) %sc% "( >= 1 & <= 10 ) | == -9 | is.na",
    c(1, 3, 9, 5, NA, -9))
  expect_equal(
    c(1, 3, 9, 5, NA, -9) %sc% "(( >= 1 & <= 10 ) | == -9) & !is.na",
    c(1, 3, 9, 5, -9))
})


test_that("identifying indices works", {
  expect_equal(5:1 %?g% 3, c(1, 2))

  expect_equal(5:1 %?ge% 3, c(1, 2, 3))

  expect_equal(5:1 %?l% 3, c(4, 5))

  expect_equal(5:1 %?le% 3, c(3, 4, 5))

  expect_equal(5:1 %?gl% c(2, 4), c(3))

  expect_equal(5:1 %?gle% c(2, 4), c(2, 3))

  expect_equal(5:1 %?gele% c(2, 4), c(2, 3, 4))

  expect_equal(5:1 %?gel% c(2, 4), c(3, 4))

  expect_equal(5:1 %?in% c(2, 4, 6), c(2, 4))

  expect_equal(5:1 %?!in% c(2, 4, 6), c(1, 3, 5))

  expect_equal(5:1 %?nin% c(2, 4, 6), c(1, 3, 5))

  expect_equal(11:15 %?==% c(11, 1, 13, 15, 15), c(1, 3, 5))

  expect_equal(11:15 %?!=% c(11, 1, 13, 15, 15), c(2, 4))

  expect_equal(
    c(1, 3, 9, 5, NA, -9) %?c% "( >= 1 & <= 10 ) | == -9",
    c(1, 2, 3, 4, 6))
  expect_equal(
    c(1, 3, 9, 5, NA, -9) %?c% "( >= 1 & <= 10 ) | == -9 | is.na",
    c(1, 2, 3, 4, 5, 6))
  expect_equal(
    c(1, 3, 9, 5, NA, -9) %?c% "(( >= 1 & <= 10 ) | == -9) & !is.na",
    c(1, 2, 3, 4, 6))

})


test_that("so-called all logical comparisons work", {
  expect_false(5:1 %ag% 3)
  expect_true(5:1 %ag% 0)

  expect_false(5:1 %age% 3)
  expect_true(5:1 %age% 1)

  expect_false(5:1 %al% 3)
  expect_true(5:1 %al% 6)

  expect_false(5:1 %ale% 3)
  expect_true(5:1 %ale% 5)

  expect_false(5:1 %agl% c(2, 4))
  expect_false(5:1 %agl% c(2, 6))
  expect_false(5:1 %agl% c(0, 4))
  expect_true(5:1 %agl% c(0, 6))

  expect_false(5:1 %agle% c(2, 4))
  expect_false(5:1 %agle% c(0, 4))
  expect_false(5:1 %agle% c(1, 5))
  expect_true(5:1 %agle% c(0, 5))

  expect_false(5:1 %agele% c(2, 4))
  expect_false(5:1 %agele% c(2, 5))
  expect_false(5:1 %agele% c(1, 4))
  expect_true(5:1 %agele% c(1, 5))

  expect_false(5:1 %agel% c(2, 4))
  expect_false(5:1 %agel% c(1, 4))
  expect_false(5:1 %agel% c(2, 6))
  expect_true(5:1 %agel% c(1, 6))

  expect_false(5:1 %ain% c(2, 4, 6))
  expect_true(5:1 %ain% 1:10)

  expect_false(5:1 %a!in% c(2, 4, 6))
  expect_false(5:1 %a!in% c(6:10, 1))
  expect_true(5:1 %a!in% c(6:11))

  expect_false(5:1 %anin% c(2, 4, 6))
  expect_false(5:1 %anin% c(6:10, 1))
  expect_true(5:1 %anin% c(6:11))

  expect_true(1:5 %a==% 1:5)
  expect_false(1:5 %a==% 5:1)

  expect_false(1:5 %a!=% 1:5)
  expect_false(1:5 %a!=% 5:1)
  expect_true(1:5 %a!=% c(5, 4, 1, 3, 2))

  expect_equal(
    c(1, 3, 9, 5, NA, -9) %ac% "( >= 1 & <= 10 ) | == -9",
    NA)
  expect_true(
    c(1, 3, 9, 5, NA, -9) %ac% "( >= 1 & <= 10 ) | == -9 | is.na")
  expect_false(
    c(1, 3, 9, 5, NA, -9) %ac% "(( >= 1 & <= 10 ) | == -9) & !is.na")

})

## https://en.wikipedia.org/wiki/Interval_(mathematics)
