#' Majoirty undersampling strategy
#'
#' @export
majority_undersampling <- function(x, target) {
  y <- x[[target]]
  t <- table(y)
  minority_class_idx <- which.min(t)
  majority_class_idx <- which.max(t)
  minority_class <- names(t)[minority_class_idx]
  majority_class <- names(t)[majority_class_idx]
  
  slice_minority <- x[y == minority_class,]
  
  slice_majority <- x[y == majority_class,]
  s <- sample(1:nrow(slice_majority), nrow(slice_minority), replace = FALSE)
  undersampling_majority <- slice_majority[s,]
  
  rbind(slice_minority, undersampling_majority)
}
