#' glm model callback
#'
#' @export
glm_model <- function(xtrain, ytrain, opts) {
  if (!require("stats")) {
    stop("'glm_model' function requires 'stats' package")
  }
  
  ytrain <- as.numeric(as.character(ytrain))
  data <- cbind(ytrain, xtrain)
  colnames(data) <- c("Class", colnames(xtrain))
  fmla <- as.formula("Class~.")
  f <- get("glm", asNamespace("stats"))
  do.call(f, c(list(formula=fmla, data=data), opts))
}

#' glm predict callback
#'
#' @export
glm_predict <- function(model, xtest, ytest, opts) {
  if (!require("stats")) {
    stop("'glm_predict' function requires 'stats' package")
  }
  
  if (!require("ROCR")) {
    stop("'glm_predict' function requires 'ROCR' package")
  }
  
  ytest <- as.numeric(as.character(ytest))
  f <- get("predict", asNamespace("stats"))
  prob <- do.call(f, c(list(object=model, newdata=xtest), opts))
  ROCR::prediction(as.vector(prob), ytest)
}

#' rf model callback
#' 
#' @export
rf_model <- function(xtrain, ytrain, opts) {
  if (!require("randomForest")) {
    stop("'rf_model' function requires 'randomForest' package")
  }
  
  do.call("randomForest", c(list(xtrain, ytrain), opts))
}

#' rf predict callback
#' 
#' @export
rf_predict <- function(model, xtest, ytest, opts) {
  if (!require("randomForest")) {
    stop("'rf_predict' function requires 'randomForest' package")
  }
  
  if (!require("ROCR")) {
    stop("'rf_predict' function requires 'ROCR' package")
  }
  
  if (is.null(opts$class.colname)) {
    stop("'rf_predict' function requires an option 'class.colname' to be defined")
  }
  
  prob <- do.call("predict", c(list(object=model, newdata=xtest), opts))
  prediction(prob[,which(colnames(prob) == opts$class.colname)], ytest)
}

#' rpart model callback
#' 
#' @export
rpart_model <- function(xtrain, ytrain, opts) {
  if (!require("rpart")) {
    stop("'rpart_model' function requires 'rpart' package")
  }
  
  data <- cbind(ytrain, xtrain)
  colnames(data) <- c("Class", colnames(xtrain))
  fmla <- as.formula("Class~.")
  do.call("rpart", c(list(formula=fmla, data=data), opts))
}

#' rpart predict callback
#' 
#' @export
rpart_predict <- function(model, xtest, ytest, opts) {
  if (!require("rpart")) {
    stop("'rpart_predict' function requires 'rpart' package")
  }
  
  if (!require("ROCR")) {
    stop("'rpart_predict' function requires 'ROCR' package")
  }
  
  if (is.null(opts$class.colname)) {
    stop("'rpart_predict' function requires an option 'class.colname' to be defined")
  }
  
  prob <- do.call("predict", c(list(object=model, newdata=xtest), opts))
  prediction(prob[,which(colnames(prob) == opts$class.colname)], ytest)
}

#' ctree model callback
#' 
#' @export
ctree_model <- function(xtrain, ytrain, opts) {
  if (!require("party")) {
    stop("'ctree_model' function requires 'party' package")
  }
  
  data <- cbind(ytrain, xtrain)
  colnames(data) <- c("Class", colnames(xtrain))
  fmla <- as.formula("Class~.")
  do.call("ctree", c(list(formula=fmla, data=data), opts))
}

#' ctree predict callback
#' 
#' @export
ctree_predict <- function(model, xtest, ytest, opts) {
  if (!require("party")) {
    stop("'ctree_predict' function requires 'party' package")
  }
  
  if (!require("ROCR")) {
    stop("'ctree_predict' function requires 'ROCR' package")
  }
  
  res <- do.call("predict", c(list(object=model, newdata=xtest), opts))
  prob <- do.call("rbind", res)
  prediction(prob[,2], ytest)
}

#' xgboost model callback
#' 
#' @export
xgboost_model <- function(xtrain, ytrain, opts) {
  if (!require("xgboost"))
    stop("'xgboost_model' function requires 'xgboost' package")
  
  xtrain <- as.matrix(xtrain)
  ytrain <- as.numeric(as.character(ytrain))
  do.call("xgboost", c(list(data=xtrain, label=ytrain), opts))
}

#' xgboost predict callback
#' 
#' @export
xgboost_predict <- function(model, xtest, ytest, opts) {
  if (!require("xgboost")) {
    stop("'xgboost_predict' function requires 'xgboost' package")
  }
  
  if (!require("ROCR")) {
    stop("'xgboost_predict' function requires 'ROCR' package")
  }
  
  xtest <- as.matrix(xtest)
  ytest <- as.numeric(as.character(ytest))
  prob <- do.call("predict", c(list(object=model, newdata=xtest), opts))
  prediction(prob, ytest)
}

#' gbm3 model callback
#'
#' @export
gbm3_model <- function(xtrain, ytrain, opts) {
  if (!require("gbm3")) {
    stop("'gbm3_model' function requires 'gbm3' package")
  }
  
  f <- get("gbm.fit", asNamespace("gbm3"))
  do.call(f, c(list(x=xtrain, y=ytrain), opts))
}

#' gbm3 predict callback
#'
#' @export
gbm3_predict <- function(model, xtest, ytest, opts) {
  if (!require("gbm3")) {
    stop("'gbm3_predict' function requires 'gbm3' package")
  }
  
  if (!require("ROCR")) {
    stop("'glm_predict' function requires 'ROCR' package")
  }
  
  f <- get("predict", asNamespace("gbm3"))
  prob <- do.call(f, c(list(object=model, newdata=xtest), opts))
  ROCR::prediction(prob, ytest)
}

#' knn model callback
#'
#' @export
knn_model <- function(xtrain, ytrain, opts) {
  if (!require("class")) {
    stop("'knn_model' function requires 'class' package")
  }
  
  list(xtrain=xtrain, ytrain=ytrain)
}

#' knn predict callback
#'
#' @export
knn_predict <- function(model, xtest, ytest, opts) {
  if (!require("class")) {
    stop("'knn_predict' function requires 'class' package")
  }
  
  if (!require("ROCR")) {
    stop("'glm_predict' function requires 'ROCR' package")
  }
  
  f <- get("knn", asNamespace("class"))
  prob <- attr(do.call(f, c(list(train=model$xtrain, test=xtest, cl=model$ytrain), opts)), "prob")
  ROCR::prediction(prob, ytest)
}

#' lda model callback
#'
#' @export
lda_model <- function(xtrain, ytrain, opts) {
  if (!require("MASS")) {
    stop("'lda_model' function requires 'MASS' package")
  }
  
  data <- cbind(ytrain, xtrain)
  colnames(data) <- c("Class", colnames(xtrain))
  fmla <- as.formula("Class~.")
  f <- get("lda", asNamespace("MASS"))
  do.call(f, c(list(formula=fmla, data=data), opts))
}

#' lda predict callback
#'
#' @export
lda_predict <- function(model, xtest, ytest, opts) {
  if (!require("MASS")) {
    stop("'lda_predict' function requires 'MASS' package")
  }
  
  if (!require("ROCR")) {
    stop("'lda_predict' function requires 'ROCR' package")
  }
  
  if (is.null(opts$class.colname)) {
    stop("'lda_predict' function requires an option 'class.colname' to be defined")
  }
  
  f <- get("predict", asNamespace("MASS"))
  p <- do.call(f, c(list(object=model, newdata=xtest), opts))
  prob <- as.vector(p$posterior[,which(colnames(p$posterior) == opts$class.colname)])
  ROCR::prediction(prob, ytest)
}

#' nb model callback
#'
#' @export
nb_model <- function(xtrain, ytrain, opts) {
  if (!require("e1071")) {
    stop("'nb_model' function requires 'e1071' package")
  }
  
  data <- cbind(ytrain, xtrain)
  colnames(data) <- c("Class", colnames(xtrain))
  fmla <- as.formula("Class~.")
  f <- get("naiveBayes", asNamespace("e1071"))
  do.call(f, c(list(formula=fmla, data=data), opts))
}

#' nb predict callback
#'
#' @export
nb_predict <- function(model, xtest, ytest, opts) {
  if (!require("e1071")) {
    stop("'nb_predict' function requires 'e1071' package")
  }
  
  if (!require("ROCR")) {
    stop("'nb_predict' function requires 'ROCR' package")
  }
  
  if (is.null(opts$class.colname)) {
    stop("'nb_predict' function requires an option 'class.colname' to be defined")
  }
  
  f <- get("predict", asNamespace("e1071"))
  prob <- do.call(f, c(list(object=model, newdata=xtest), opts))
  ROCR::prediction(prob[,which(colnames(prob) == opts$class.colname)], ytest)
}

#' svm model callback
#'
#' @export
svm_model <- function(xtrain, ytrain, opts) {
  if (!require("e1071")) {
    stop("'svm_model' function requires 'e1071' package")
  }
  
  f <- get("svm", asNamespace("e1071"))
  do.call(f, c(list(x=xtrain, y=ytrain), opts))
}

#' svm predict callback
#'
#' @export
svm_predict <- function(model, xtest, ytest, opts) {
  if (!require("e1071")) {
    stop("'svm_predict' function requires 'e1071' package")
  }
  
  if (!require("ROCR")) {
    stop("'svm_predict' function requires 'ROCR' package")
  }
  
  if (is.null(opts$class.colname)) {
    stop("'svm_predict' function requires an option 'class.colname' to be defined")
  }
  
  f <- get("predict", asNamespace("e1071"))
  prob <- do.call(f, c(list(object=model, newdata=xtest), opts))
  p.df <- attributes(prob)$probabilities
  p.good <- p.df[,which(colnames(p.df) == opts$class.colname)]
  ROCR::prediction(as.vector(p.good), ytest)
}
#' nnet model callback
#'
#' @export
nnet_model <- function(xtrain, ytrain, opts) {
  if (!require("nnet")) {
    stop("'nnet_model' function requires 'nnet' package")
  }
  
  data <- cbind(ytrain, xtrain)
  colnames(data) <- c("Class", colnames(xtrain))
  fmla <- as.formula("Class~.")
  f <- get("nnet", asNamespace("nnet"))
  do.call(f, c(list(formula=fmla, data=data), opts))
}

#' nnet predict callback
#'
#' @export
nnet_predict <- function(model, xtest, ytest, opts) {
  if (!require("nnet")) {
    stop("'nnet_predict' function requires 'nnet' package")
  }
  
  if (!require("ROCR")) {
    stop("'nnet_predict' function requires 'ROCR' package")
  }
  
  f <- get("predict", asNamespace("nnet"))
  prob <- do.call(f, c(list(object=model, newdata=xtest), opts))
  ROCR::prediction(prob[,1], ytest)
}
