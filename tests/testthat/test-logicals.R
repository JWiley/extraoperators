test_that("logical comparisons work", {
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
  expect_equal(1 %nin% c(1, 2), FALSE)
  expect_equal(1 %nin% c(3, 2), TRUE)
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
})


