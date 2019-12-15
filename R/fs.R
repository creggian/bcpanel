#' mRMRe feature selection callback
#' 
#' @export
mRMRe_fs_callback <- function(xtrain, ytrain, opts) {
  check_if_loaded(
    package = "mRMRe",
    stop_message = "'mRMRe_fs_callback' function requires 'mRMRe' package. Please load it."
  )
  
  y <- as_charnum(ytrain)
  train_df <- cbind(
    data.frame(y = y),
    xtrain
  )
  
  dd <- mRMRe::mRMR.data(data = train_df)
  
  res <- mRMRe::mRMR.ensemble(
    data = dd,
    target_indices = 1,
    solution_count = opts$solution_count,
    feature_count = opts$feature_count
  )
  
  selected_features_names <- apply(mRMRe::solutions(res)[[1]], 2, function(x, y) { return(y[x]) }, y = mRMRe::featureNames(dd))[,1]
  
  list(
    selected_features_names = selected_features_names
  )
}

#' Correlation feature selection callback
#' 
#' @export
corr_fs_callback <- function(xtrain, ytrain, opts) {
  check_if_loaded(
    package = "stats",
    stop_message = "'corr_fs_callback' function requires 'stats' package. Please load it."
  )
  
  y <- ytrain %>% as_charnum()
  c <- cor(xtrain, y)
  
  rank <- order(abs(c), decreasing = TRUE)
  ordered_variables <- colnames(xtrain)[rank]
  selected_features_names <- ordered_variables[1:opts$feature_count]
  
  list(
    selected_features_names = selected_features_names
  )
}

#' Boruta feature selection callback
#' 
#' @export
boruta_fs_callback <- function(xtrain, ytrain, opts) {
  check_if_loaded(
    package = "Boruta",
    stop_message = "'boruta_fs_callback' function requires 'Boruta' package. Please load it."
  )
  
  train_df <- cbind(
    data.frame(Class = ytrain),
    xtrain
  )
  
  if (!is.list(opts)) {
    opts <- list()
  }
  if (is.null(opts$p_value)) {
    opts$p_value <- 0.01
  }
  
  boruta_selection <- Boruta::Boruta(
    Class ~ .,
    data = train_df,
    pValue = opts$p_value
  )
  selected_features_names <- Boruta::getSelectedAttributes(boruta_selection)
  
  list(
    selected_features_names = selected_features_names
  )
}