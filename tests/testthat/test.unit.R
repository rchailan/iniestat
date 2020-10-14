context("Read files")

test_that("f24_read", {
  # no arguments should imply raising an error
  expect_error(iniestat::read_f24())
})
