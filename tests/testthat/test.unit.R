context("Read files")

test_that("f24_read-error", {
  # no arguments should imply raising an error
  testthat::expect_error(iniestat::read_f24())
})

test_that("f24_read-example", {
  testthat::expect_equal(
    iniestat::read_f24(file_path = system.file("extdata", "f24","Bolton_ManCityF24.xml", package = "iniestat")) %>%
      nrow(),
    6718
  )
})

