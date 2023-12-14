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

  expect_equal(
    c(1, 3, 9, 5, -9) %se% "(-8, 1] | [2, 9)",
    c(1, 3, 5))

  expect_equal(
    c("jack", "jane", "ajay") %sgrepl% "ja",
    c("jack", "jane", "ajay")
  )
  expect_equal(
    c("jack", "jill", "john", "jane", "sill", "ajay") %sgrepl% "^ja",
    c("jack", "jane")
  )
  expect_equal(
    c("jack", "jill", "john", "jane", "sill", "ajay") %sgrepl% "ja$",
    character(0)
  )

  expect_equal(
    c("jack", "jane", "ajay") %s!grepl% "ja",
    character(0)
  )
  expect_equal(
    c("jack", "jill", "john", "jane", "sill", "ajay") %s!grepl% "^ja",
    c("jill", "john", "sill", "ajay")
  )
  expect_equal(
    c("jack", "jill", "john", "jane", "sill", "ajay") %s!grepl% "ja$",
    c("jack", "jill", "john", "jane", "sill", "ajay")
  )
})

