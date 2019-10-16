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

  expect_false(c(1, 3, 9, 5, -9) %ae% "(-8, 1] | [2, 9)")
  expect_true(c(1, 3, 9, 5, -9) %ae% "(-Inf, Inf)")

})

## https://en.wikipedia.org/wiki/Interval_(mathematics)
