#' get formula
#'
#' @export
get_formula <- function(col.names, class.idx=1) {
  class.name <- col.names[class.idx]
  as.formula(paste(class.name, " ~ ", paste(col.names[-class.idx], collapse=" + ")))
}

#' As charnum
#'
#' @export
as_charnum <- function(x) {
  as.numeric(as.character(x))
}

#' Scaling function
#'
#' @export
scale_this <- function(x, method="scale") {
  ret <- NULL
  if (method == "std") ret <- (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  if (method == "norm") ret <- (x - mean(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
  if (method == "scale") ret <- (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
  ret
}