#' Table performance
#' 
#' @export
table.performance <- function(classifiers, measure, return_x_measure=FALSE, set_x=NULL, set_x_inequality=">=", labels=NULL) {
  if (!require("ROCR"))
    stop("'table.performance' function requires 'ROCR' package")
  
  if (is.null(labels)) {
    labels <- sapply(classifiers, function(x) {x$label})
  }
  
  m <- sapply(classifiers, function(x) {
    pred <- prediction(x$predictions, x$truth)
    perf <- performance(pred, measure=measure)
    x.values <- perf@x.values
    if (length(x.values) > 0) {
      x.values <- x.values[[1]]
    }
    y.values <- perf@y.values[[1]]
    
    ret <- NA
    if (is.numeric(set_x)) {
      if (is.numeric(x.values)) {
        if (set_x_inequality == ">=") {
          pos <- max(which(x.values >= set_x))
        } else {
          pos <- min(which(x.values <= set_x))
        }
        ret <- y.values[pos]
      }
    } else {
      ret <- max(y.values)
      if (return_x_measure) {
        ret <- x.values[which.max(y.values)]
      }
    }
    ret
  })
  m.df <- data.frame(t(m))
  colnames(m.df) <- labels
  m.df
}

#' it returns a dataframe with auc performances
#'
#' @param   classifiers     a list, where each element contains three information
#'                          i) 'label': string representing the classifier used
#'                          ii) 'predictions': vector of numerical prediction
#'                          iii) 'truth': vector of true results
#'
#' @export auc
auc <- function(classifiers, labels=NULL) {
  table.performance(classifiers, measure="auc", labels=labels)
}