#' it plots ROC curves
#'
#' @param x.measure 'cutoff'
#' @param classifiers     a list, where each element contains three information
#'                        i) 'label': string representing the classifier used
#'                        ii) 'predictions': vector of numerical prediction
#'                        iii) 'truth': vector of true results
#'                          
#' @export plot.roc
plot.roc <- function(classifiers, ...) {
  plot.performance(classifiers, measure="tpr", x.measure="fpr", ...)
}

#' Plotting performance
#'
#' @param x.measure default 'cutoff'
#' @param classifiers     a list, where each element contains three information
#'                        i) 'label': string representing the classifier used
#'                        ii) 'predictions': vector of numerical prediction
#'                        iii) 'truth': vector of true results
#'                          
#' @export plot.performance
plot.performance <- function(classifiers, measure, x.measure="cutoff", main="", type="l", labels=NULL, ...) {
  if (!require("ROCR"))
    stop("'plot.performance' function requires 'ROCR' package")
  
  if (is.null(labels)) {
    model_names <- sapply(classifiers, function(x) {x$label})
    labels <- model_names
    
    if (measure == "tpr" & x.measure == "fpr") {
      # if roc is demanded, then we provide in the label
      # also the auc value
      aucs_t <- auc(classifiers)
      aucs <- setNames(as.vector(aucs_t), colnames(aucs_t))
      
      labels <- sapply(names(model_names), function(m) {
        paste(as.vector(model_names[m]), " (auc=", round(as.vector(aucs[m]), 3), ")", sep="")
      })
    }
  }
  
  palette <- rainbow(length(classifiers))
  for(i in 1:length(classifiers)) {
    pred <- prediction(classifiers[[i]]$predictions, classifiers[[i]]$truth)
    perf <- performance(prediction.obj=pred, measure=measure, x.measure=x.measure)
    
    if (i > 1) {
      plot(perf, col=palette[i], add=TRUE, type=type)
    } else {
      plot(perf, col=palette[i], main=main, type=type)
    }
  }
  legend("bottomright", legend=labels, pch=15, col=palette, horiz=FALSE, ...)
}