#' Majoirty undersampling strategy
#'
#' @export
majority_undersampling <- function(x, target) {
  bool <- majority_undersampling_bool(x = x, target = target)
  x[bool,]
}

#' Majority undersampling booleans
#' 
#' @export
majority_undersampling_bool <- function(x, target) {
  y <- x[[target]]
  t <- table(y)
  minority_class_idx <- which.min(t)
  majority_class_idx <- which.max(t)
  minority_class <- names(t)[minority_class_idx]
  majority_class <- names(t)[majority_class_idx]
  
  minority_bool <- y == minority_class
  
  majority_bool <- y == majority_class
  majority_idx <- which(majority_bool)
  slice_majority_idx <- sample(majority_idx, min(t), replace = FALSE)
  slice_majority_bool <- (1:nrow(x)) %in% slice_majority_idx
  
  minority_bool | slice_majority_bool
}
