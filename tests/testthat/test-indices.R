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

  expect_equal(
    c(1, 3, 9, 5, -9) %?e% "(-8, 1] | [2, 9)",
    c(1, 2, 4))

})

