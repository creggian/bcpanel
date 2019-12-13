#' bcp
#' 
#' https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/ for description of decision trees
#' 
#' @param nfolds if 'k.cv' is not null, then 'nfolds' is not used
#'
#' @export bcp
bcp <- function(x, y, folds=NULL, nfolds=10, panel, save_models=TRUE, ncores=1, debug=FALSE) {
  if (is.null(folds)) {
    if (!require("caret"))
      stop("bcp function requires 'caret' package to create folds")
    
    folds <- createFolds(y, k=nfolds, list=TRUE, returnTrain=FALSE)
    names(folds) <- NULL
  }
  
  classifiers <- lapply(seq_along(panel), function(i) {
    name <- names(panel)[i]
    if (debug) {
      message(name)
    }
    p <- panel[[i]]
    
    classif <- cv(
      x,
      y,
      folds = folds,
      model_callback = p$model_callback,
      predict_callback = p$predict_callback,
      fs_callback = p$fs_callback,
      opts = p$opts,
      ncores = ncores
    )
    idx <- lapply(classif, function(x) x[["idx"]])
    models <- lapply(classif, function(x) x[["model"]])
    preds <- lapply(classif, function(x) x[["pred"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    if (is.function(p$fs_callback)) {
      fs <- lapply(classif, function(x) x[["fs"]])
    } else {
      fs <- NULL
    }
    
    ret <- list(
      idx = idx,
      label = name,
      preds = preds,
      predictions = predictions,
      truth = truth,
      fs = fs
    )
    if (isTRUE(save_models)) {
      ret$models <- models
    }
    
    ret
  })
  
  names(classifiers) <- names(panel)

  classifiers
}