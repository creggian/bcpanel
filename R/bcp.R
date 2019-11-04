#' bcp
#' 
#' https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/ for description of decision trees
#' 
#' @param nfolds if 'k.cv' is not null, then 'nfolds' is not used
#'
#' @export bcp
bcp <- function(x, y, folds=NULL, nfolds=10, panel, save.models=TRUE, save.formulas=TRUE, ncores=1) {
  if (is.null(folds)) {
    if (!require("caret"))
      stop("bcp function requires 'caret' package to create folds")
    
    folds <- createFolds(y, k=nfolds, list=TRUE, returnTrain=FALSE)
    names(folds) <- NULL
  }
  
  classifiers <- lapply(seq_along(panel), function(idx) {
    name <- names(panel)[idx]
    p <- panel[[idx]]
    
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
    models <- lapply(classif, function(x) x[["model"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    list(
      label = name,
      predictions = predictions,
      truth = truth,
      models = models
    )
  })
  
  names(classifiers) <- names(panel)

  classifiers
}