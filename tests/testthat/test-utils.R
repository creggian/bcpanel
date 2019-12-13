context("utils")

test_that("check_if_loaded", {
  expect_equal(check_if_loaded(package = "stats"), TRUE)
  expect_equal(check_if_loaded(package = "mRMRe"), FALSE)
  expect_equal(check_if_loaded(package = "wrongpackage"), FALSE)
  
  expect_error(check_if_loaded(package = "mRMRe", stop_message = "message"))
  expect_error(check_if_loaded(package = "wrongpackage", stop_message = "message"))
  
  library("mRMRe")
  expect_equal(check_if_loaded(package = "mRMRe"), TRUE)
})