context("fs")

test_that("mRMRe_fs_callback", {
  library("datasets")
  
  df <- mtcars
  x <- df[, setdiff(colnames(df), "am")]
  y <- factor(df$am)
  
  n_features <- 2
  opts <- list(
    solution_count = 1,
    feature_count = n_features
  )
  
  if ("mRMRe" %in% (.packages())) {
    detach("package:mRMRe", unload=TRUE)
  }
  expect_error(mRMRe_fs_callback(x, y, opts))
  
  library("mRMRe")
  object <- mRMRe_fs_callback(x, y, opts)
  expect_equal(length(object$selected_features_names), n_features)
  expect_equal(class(object$selected_features_names), "character")
  expect_equal(
    sort(intersect(object$selected_features_names, colnames(df))),
    sort(object$selected_features_names)
  )
  
  detach("package:mRMRe", unload=TRUE)
})

test_that("corr_fs_callback", {
  library("datasets")
  
  df <- mtcars
  x <- df[, setdiff(colnames(df), "am")]
  y <- factor(df$am)
  
  n_features <- 2
  opts <- list(
    feature_count = n_features
  )
  
  object <- corr_fs_callback(x, y, opts)
  expect_equal(length(object$selected_features_names), n_features)
  expect_equal(class(object$selected_features_names), "character")
  expect_equal(
    sort(intersect(object$selected_features_names, colnames(df))),
    sort(object$selected_features_names)
  )
  expect_equal(
    sort(object$selected_features_names),
    c("drat", "gear")
  )
})

test_that("boruta_fs_callback", {
  library("datasets")
  
  df <- mtcars
  x <- df[, setdiff(colnames(df), "am")]
  y <- factor(df$am)
  
  n_features <- 2
  opts <- list(
    p_value = 0.01
  )
  
  if ("Boruta" %in% (.packages())) {
    detach("package:Boruta", unload=TRUE)
  }
  expect_error(boruta_fs_callback(x, y, NULL))
  expect_error(boruta_fs_callback(x, y, opts))
  
  library("Boruta")
  object <- boruta_fs_callback(x, y, opts)
  expect_equal(class(object$selected_features_names), "character")
  expect_equal(
    sort(intersect(object$selected_features_names, colnames(df))),
    sort(object$selected_features_names)
  )
  
  object <- boruta_fs_callback(x, y, NULL)
  expect_equal(class(object$selected_features_names), "character")
  expect_equal(
    sort(intersect(object$selected_features_names, colnames(df))),
    sort(object$selected_features_names)
  )
  
  detach("package:Boruta", unload=TRUE)
})



