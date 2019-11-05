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