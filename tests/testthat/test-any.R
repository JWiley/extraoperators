test_that("any logical comparisons work", {
  expect_true(5:1 %anyg% 3)
  expect_false(5:1 %anyg% 5)
  expect_equal(c(NA, 1) %anyg% 2, NA)

  expect_true(5:1 %anyge% 5)
  expect_false(5:1 %anyge% 6)

  expect_true(5:1 %anyl% 3)
  expect_false(5:1 %anyl% 1)

  expect_true(5:1 %anyle% 1)
  expect_false(5:1 %anyle% 0)

  expect_true(5:1 %anygl% c(2, 4))
  expect_false(5:1 %anygl% c(5, 6))

  expect_true(5:1 %anygle% c(2, 4))
  expect_false(5:1 %anygle% c(5, 6))

  expect_true(5:1 %anygele% c(2, 4))
  expect_false(5:1 %anygele% c(6, 7))

  expect_true(5:1 %anygel% c(2, 4))
  expect_false(5:1 %anygel% c(6, 7))

  expect_true(5:1 %anyin% c(2, 4, 6))
  expect_false(5:1 %anyin% 6:10)

  expect_true(5:1 %any!in% c(2, 4, 6))
  expect_false(5:1 %any!in% 1:5)
  expect_true(5:1 %anynin% c(2, 4, 6))
  expect_false(5:1 %anynin% 1:5)

  expect_true(1:5 %any==% 5:1)
  expect_false(1:5 %any==% 6:10)
  expect_equal(c(NA, 1) %any==% 2:3, NA)

  expect_true(1:5 %any!=% 5:1)
  expect_false(1:5 %any!=% 1:5)

  expect_true(c(1, 3, 9, 5, NA, -9) %anyc% "( >= 1 & <= 10 ) | == -9")
  expect_false(c(20, 30) %anyc% "> 100")
  expect_equal(c(NA, 20) %anyc% "> 100", NA)

  expect_true(c(1, 3, 9, 5, -9) %anye% "(-8, 1] | [2, 9)")
  expect_false(c(-9, 10) %anye% "(-8, 1] | [2, 9)")
  expect_equal(c(NA_real_, 10) %anye% "(0, 5)", NA)

  expect_true(c("jack", "jane", "ajay") %anygrepl% "ja")
  expect_false(c("jill", "john", "sill") %anygrepl% "ja")

  expect_true(c("jack", "jill", "john", "jane") %any!grepl% "^ja")
  expect_false(c("jack", "jane", "ajay") %any!grepl% "ja")

  expect_true(c("a:b", "c:d") %anyflipIn% "b:a")
  expect_false(c("a:b", "c:d") %anyflipIn% "x:y")
})

test_that("any logical comparisons inherit validation", {
  expect_error(1 %anygele% 1)
  expect_error(1 %anygele% c(1, NA_real_))
  expect_error(1 %any!in% numeric(0))
  expect_error("text" %anygrepl% "")
  expect_error("text" %any!grepl% c("a", "b"))
})
