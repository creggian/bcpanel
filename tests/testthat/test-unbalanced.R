context("unbalanced")

test_that("majority_undersampling", {
  df <- data.frame(
    c1 = c(1, 2, 3, 4, 5.8),
    c2 = c("a", "b", "c", "d", "e"),
    y = factor(c(1, 1, 0, 0, 0)),
    stringsAsFactors = FALSE
  )
  
  object <- majority_undersampling(x = df, target = "y")
  expect_equal(class(object), "data.frame")
  expect_equal(nrow(object), 4)
  expect_equal(ncol(object), 3)
  expect_equal(length(which(object$y == 1)), 2)
  expect_equal(length(which(object$y == 0)), 2)
  
  df <- data.frame(
    c1 = c(1, 2, 3, 4, 5.8),
    c2 = c("a", "b", "c", "d", "e"),
    y = factor(c(1, 1, 1, 1, 0)),
    stringsAsFactors = FALSE
  )
  
  object <- majority_undersampling(x = df, target = "y")
  expect_equal(class(object), "data.frame")
  expect_equal(nrow(object), 2)
  expect_equal(ncol(object), 3)
  expect_equal(length(which(object$y == 1)), 1)
  expect_equal(length(which(object$y == 0)), 1)
})

test_that("majority_undersampling_bool", {
  df <- data.frame(
    c1 = c(1, 2, 3, 4, 5.8),
    c2 = c("a", "b", "c", "d", "e"),
    y = factor(c(1, 1, 0, 0, 0)),
    stringsAsFactors = FALSE
  )
  
  object <- majority_undersampling_bool(x = df, target = "y")
  expect_equal(class(object), "logical")
  expect_equal(length(object), 5)
  expect_equal(length(which(object == TRUE)), 4)
  expect_equal(length(which(object == FALSE)), 1)
  
  df <- data.frame(
    c1 = c(1, 2, 3, 4, 5.8),
    c2 = c("a", "b", "c", "d", "e"),
    y = factor(c(1, 1, 1, 1, 0)),
    stringsAsFactors = FALSE
  )
  
  object <- majority_undersampling_bool(x = df, target = "y")
  expect_equal(class(object), "logical")
  expect_equal(length(object), 5)
  expect_equal(length(which(object == TRUE)), 2)
  expect_equal(length(which(object == FALSE)), 3)
})
