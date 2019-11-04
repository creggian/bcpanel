#' Cross validation function
#'
#' @param x a data.frame or matrix of predictors
#' @param y response vector
#' @param nfolds number of folds in cv, set k=nrow(df) to have loo
#' @param folds list created with caret::createFolds function
#' @param model_callback
#' @param predict_callback
#' @param fs_callback number of new (training) dataset sampled with replacement from 'training'
#' @param ncores if ncores > 1 then it works in parallel
#'
#' @return list a (nested) list of ROCR prediction objects
#' 
#' @export cv
cv <- function(x, y, nfolds=10, folds=NULL, model_callback, predict_callback, fs_callback=NULL, opts=list(), ncores=1) {
  if (is.null(folds)) {
    if (!require("caret"))
      stop("'cv' function requires 'caret' package")
    
    folds <- createFolds(y, k=nfolds, list=TRUE, returnTrain=FALSE)
    names(folds) <- NULL
  }
  
  # init opts if not set
  if (!is.list(opts)) {
    opts <- list()
  }
  if (is.null(opts$model)) {
    opts$model <- list()
  }
  if (is.null(opts$predict)) {
    opts$predict <- list()
  }
  if (is.null(opts$fs)) {
    opts$fs <- list()
  }
  
  cvs <- mclapply(folds, function(idx) {
    if (length(folds) > 1) {
      # if we have more than one fold, then we perform
      # cross validation as expected.
      testing_data <- x[idx,]
      testing_label <- y[idx]
      training_data <- x[-idx,]
      training_label <- y[-idx]
      
      if (!is.null(fs_callback)) {
        fs <- fs_callback(xtrain=training_data, ytrain=training_label, opts$fs)
        training_data <- fs$xtrain
        training_label <- fs$ytrain
      }
    } else {
      # if we have only one fold, that means that we
      # want to build the model using the whole dataset;
      # in this case the prediction will be on the training
      # dataset.
      testing_data <- x[idx,]
      testing_label <- y[idx]
      training_data <- testing_data
      training_label <- testing_label
    }
    model <- model_callback(xtrain=training_data, ytrain=training_label, opts$model)
    pred <- predict_callback(model, xtest=testing_data, ytest=testing_label, opts$predict)
    list(model=model, pred=pred)
  }, mc.cores=ncores)
  cvs
}