test_that("logical comparisons work", {
  expect_equal(1 %g% 0, TRUE)
  expect_equal(1 %g% 1, FALSE)

  expect_equal(1 %ge% 0, TRUE)
  expect_equal(1 %ge% 1, TRUE)
  expect_equal(1 %ge% 2, FALSE)

  expect_equal(1 %l% 0, FALSE)
  expect_equal(1 %l% 1, FALSE)
  expect_equal(1 %l% 2, TRUE)

  expect_equal(1 %le% 0, FALSE)
  expect_equal(1 %le% 1, TRUE)
  expect_equal(1 %le% 2, TRUE)

  expect_equal(1 %gele% c(1, 3), TRUE)
  expect_equal(1 %gele% c(2, 3), FALSE)

  expect_equal(1 %gle% c(0, 3), TRUE)
  expect_equal(1 %gle% c(1, 3), FALSE)

  expect_equal(1 %gel% c(0, 2), TRUE)
  expect_equal(1 %gel% c(0, 1), FALSE)

  expect_equal(1 %gl% c(0, 2), TRUE)
  expect_equal(1 %gl% c(1, 2), FALSE)
  expect_equal(1 %gl% c(0, 1), FALSE)

  expect_equal(1 %in% c(1, 2), TRUE)
  expect_equal(1 %in% c(3, 2), FALSE)

  expect_equal(1 %!in% c(3, 2), TRUE)
  expect_equal(1 %!in% c(1, 2), FALSE)
  expect_equal(1 %nin% c(3, 2), TRUE)
  expect_equal(1 %nin% c(1, 2), FALSE)

  expect_equivalent(c("a:b", "c:d") %flipIn% "b:a", c(TRUE, FALSE))
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
})


test_that("so-called all logical comparisons work", {
  expect_equal(5:1 %ag% 3, FALSE)
  expect_equal(5:1 %ag% 0, TRUE)

  expect_equal(5:1 %age% 3, FALSE)
  expect_equal(5:1 %age% 1, TRUE)

  expect_equal(5:1 %al% 3, FALSE)
  expect_equal(5:1 %al% 6, TRUE)

  expect_equal(5:1 %ale% 3, FALSE)
  expect_equal(5:1 %ale% 5, TRUE)

  expect_equal(5:1 %agl% c(2, 4), FALSE)
  expect_equal(5:1 %agl% c(2, 6), FALSE)
  expect_equal(5:1 %agl% c(0, 4), FALSE)
  expect_equal(5:1 %agl% c(0, 6), TRUE)

  expect_equal(5:1 %agle% c(2, 4), FALSE)
  expect_equal(5:1 %agle% c(0, 4), FALSE)
  expect_equal(5:1 %agle% c(1, 5), FALSE)
  expect_equal(5:1 %agle% c(0, 5), TRUE)

  expect_equal(5:1 %agele% c(2, 4), FALSE)
  expect_equal(5:1 %agele% c(2, 5), FALSE)
  expect_equal(5:1 %agele% c(1, 4), FALSE)
  expect_equal(5:1 %agele% c(1, 5), TRUE)

  expect_equal(5:1 %agel% c(2, 4), FALSE)
  expect_equal(5:1 %agel% c(1, 4), FALSE)
  expect_equal(5:1 %agel% c(2, 6), FALSE)
  expect_equal(5:1 %agel% c(1, 6), TRUE)

  expect_equal(5:1 %ain% c(2, 4, 6), FALSE)
  expect_equal(5:1 %ain% 1:10, TRUE)

  expect_equal(5:1 %a!in% c(2, 4, 6), FALSE)
  expect_equal(5:1 %a!in% c(6:10, 1), FALSE)
  expect_equal(5:1 %a!in% c(6:11), TRUE)

  expect_equal(5:1 %anin% c(2, 4, 6), FALSE)
  expect_equal(5:1 %anin% c(6:10, 1), FALSE)
  expect_equal(5:1 %anin% c(6:11), TRUE)
})


