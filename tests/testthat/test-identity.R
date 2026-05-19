test_that("strict identity operators wrap identical", {
  expect_true(1:3 %===% 1:3)
  expect_false(1 %===% 1L)
  expect_false(structure(1, names = "a") %===% 1)

  expect_false(1:3 %!==% 1:3)
  expect_true(1 %!==% 1L)
  expect_true(structure(1, names = "a") %!==% 1)
})
