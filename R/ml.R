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
cv <- function(x, y, nfolds=10, folds=NULL, model_callback, predict_callback, fs_callback=NULL, ncores=1) {
  if (is.null(folds)) {
    if (!require("caret"))
      stop("'cv' function requires 'caret' package")
    
    folds <- createFolds(y, k=nfolds, list=TRUE, returnTrain=FALSE)
    names(folds) <- NULL
  }
  
  cvs <- mclapply(folds, function(idx) {
    testing_data <- x[idx,]
    testing_label <- y[idx]
    training_data <- x[-idx,]
    training_label <- y[-idx]
    
    if (!is.null(fs_callback)) {
      fs <- fs_callback(xtrain=training_data, ytrain=training_label)
      training_data <- fs$xtrain
      training_label <- fs$ytrain
    }
    model <- model_callback(xtrain=training_data, ytrain=training_label)
    pred <- predict_callback(model, xtest=testing_data, ytest=testing_label)
    list(model=model, pred=pred)
  }, mc.cores=ncores)
  cvs
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
  if (!require("ROCR"))
    stop("'auc' function requires 'ROCR' package")
  
  if (is.null(labels)) {
    labels <- sapply(classifiers, function(x) {x$label})
  }
  
  auc <- sapply(classifiers, function(x) {
    pred <- prediction(x$predictions, x$truth)
    perf <- performance(pred, "auc")
    perf@y.values[[1]]
  })
  auc.df <- data.frame(auc=auc)
  rownames(auc.df) <- labels
  auc.df
}

#' it plot ROC curves
#'
#' @param   classifiers     a list, where each element contains three information
#'                          i) 'label': string representing the classifier used
#'                          ii) 'predictions': vector of numerical prediction
#'                          iii) 'truth': vector of true results
#'                          
#' @export plot.roc
plot.roc <- function(classifiers, main="", labels=NULL, ...) {
  if (!require("ROCR"))
    stop("'auc' function requires 'ROCR' package")
  
  if (is.null(labels)) {
    labels <- sapply(classifiers, function(x) {x$label})
  }
  
  palette <- rainbow(length(classifiers))
  for(i in 1:length(classifiers)) {
    pred <- prediction(classifiers[[i]]$predictions, classifiers[[i]]$truth)
    perf <- performance(pred, "tpr", "fpr")
    
    if (i > 1) {
      plot(perf, col=palette[i], add=TRUE, type="b")
    } else {
      plot(perf, col=palette[i], main=main, type="b")
    }
  }
  legend("bottomright", legend=labels, pch=15, col=palette, horiz=FALSE, ...)
}

#' bcp
#' 
#' https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/ for description of decision trees
#' 
#' @param nfolds if 'k.cv' is not null, then 'nfolds' is not used
#'
#' @export bcp
bcp <- function(x, y, folds=NULL, nfolds=10,
                panel=c("rpart", "rf", "nnet", "lda", "glm", "nb", "svm"),
                fs=NULL, opt=NULL, ..., save.models=TRUE, save.formulas=TRUE,
                ncores=1, class.colname=1) {
  if (!require("ROCR"))
    stop("bcp function requires 'ROCR' package")
  
  if (is.null(folds)) {
    if (!require("caret"))
      stop("bcp function requires 'caret' package")
    
    folds <- createFolds(y, k=nfolds, list=TRUE, returnTrain=FALSE)
    names(folds) <- NULL
  }
  
  classifiers <- list()
  
  if ("ctree" %in% panel) {
    if (!require("party"))
      stop("'ctree' classifier requires 'party' package")
    
    # decision tree
    classif <- cv(df, fmla, folds=folds,
                  model_callback=function(fmla, data, class) {
                    do.call("ctree", c(list(formula=fmla, data=data), opt$ctree))
                  },
                  predict_callback=function(model, newdata, class) {
                    prob <- do.call("rbind", predict(model, newdata=newdata, type="prob"))
                    prediction(prob[,2], newdata[[class]])
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    fmlas <- lapply(classif, function(x) x[["fmla"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$ctree <- list(label="ctree",
                              predictions=predictions,
                              truth=truth,
                              models=models,
                              formulas=fmlas)
  }
  
  if ("rpart" %in% panel) {
    if (!require("rpart"))
      stop("'rpart' classifier requires 'rpart' package")
    
    classif <- cv(x, y, folds=folds,
                  model_callback=function(xtrain, ytrain) {
                    data <- cbind(ytrain, xtrain)
                    colnames(data) <- c("Class", colnames(xtrain))
                    fmla <- as.formula("Class~.")
                    do.call("rpart", c(list(formula=fmla, data=data), opt$rpart))
                  },
                  predict_callback=function(model, xtest, ytest) {
                    prob <- predict(model, newdata=xtest, type="prob")
                    prediction(prob[,which(colnames(prob) == class.colname)], ytest)
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$rpart <- list(label="rpart",
                              predictions=predictions,
                              truth=truth,
                              models=models)
  }
  
  if ("rf" %in% panel) {
    if (!require("randomForest"))
      stop("'rf' classifier requires 'randomForest' package")
    
    classif <- cv(x, y, folds=folds,
                  model_callback=function(xtrain, ytrain) {
                    do.call("randomForest", c(list(xtrain, ytrain, method="rf", probability=TRUE), opt$rf))
                  },
                  predict_callback=function(model, xtest, ytest) {
                    prob <- predict(model, newdata=xtest, type="prob")
                    prediction(prob[,which(colnames(prob) == class.colname)], ytest)
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$rf <- list(label="rf",
                           predictions=predictions,
                           truth=truth,
                           models=models)
  }
  
  if ("xgboost" %in% panel) {
    if (!require("xgboost"))
      stop("'xgboost' classifier requires 'xgboost' package")
    
    classif <- cv(x, y, folds=folds,
                  model_callback=function(xtrain, ytrain) {
                    do.call("xgboost", c(list(data=xtrain, label=ytrain, objective="binary:logistic"), opt$xgboost))
                  },
                  predict_callback=function(model, xtest, ytest) {
                    prob <- predict(model, newdata=xtest)
                    prediction(prob, ytest)
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$xgboost <- list(label="xgboost",
                                predictions=predictions,
                                truth=truth,
                                models=models)
  }
  
  if ("gbm3" %in% panel) {
    if (!require("gbm3"))
      stop("'gbm3' classifier requires 'gbm3' package")
    
    classif <- cv(x, y, folds=folds,
                  model_callback=function(xtrain, ytrain) {
                    do.call("gbm.fit", c(list(x=xtrain, y=ytrain), opt$gbm3))
                  },
                  predict_callback=function(model, xtest, ytest) {
                    prob <- predict(model, newdata=xtest, n.trees=100)  # HACK!
                    prediction(prob, ytest)
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$gbm3 <- list(label="gbm3",
                             predictions=predictions,
                             truth=truth,
                             models=models)
  }
  
  if ("knn" %in% panel) {
    if (!require("class"))
      stop("'knn' classifier requires 'class' package")
    
    classif <- cv(x, y, folds=folds,
                  model_callback=function(xtrain, ytrain) {
                    list(xtrain=xtrain, ytrain=ytrain)
                  },
                  predict_callback=function(model, xtest, ytest) {
                    prob <- attr(do.call("knn", c(list(train=model$xtrain, test=xtest, cl=model$ytrain, prob=TRUE), opt$knn)), "prob")
                    prediction(prob, ytest)
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$knn <- list(label="knn",
                            predictions=predictions,
                            truth=truth,
                            models=models)
  }
  
  if ("lda" %in% panel) {
    if (!require("MASS"))
      stop("'lda' classifier requires 'MASS' package")
    
    # lda
    classif <- cv(df, fmla, folds=folds,
                  model_callback=function(fmla, data, class) {
                    do.call("lda", c(list(formula=fmla, data=data), opt$lda))
                  },
                  predict_callback=function(model, newdata, class) {
                    p <- predict(model, newdata=newdata)
                    prob <- as.vector(p$posterior[,which(colnames(p$posterior) == class.colname)])
                    pred <- prediction(prob, newdata[[class]])
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    fmlas <- lapply(classif, function(x) x[["fmla"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$lda <- list(label="lda",
                            predictions=predictions,
                            truth=truth,
                            models=models,
                            formulas=fmlas)
  }
  
  if ("nb" %in% panel) {
    if (!require("e1071"))
      stop("'nb' classifier requires 'e1071' package")
    
    # naive bayes
    classif <- cv(df, fmla, folds=folds,
                  model_callback=function(fmla, data, class) {
                    do.call("naiveBayes", c(list(formula=fmla, data=data), opt$nb))
                  },
                  predict_callback=function(model, newdata, class) {
                    prob <- predict(model, newdata, type="raw")
                    pred <- prediction(prob[,which(colnames(prob) == class.colname)], newdata[[class]])
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    fmlas <- lapply(classif, function(x) x[["fmla"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$nb <- list(label="nb",
                           predictions=predictions,
                           truth=truth,
                           models=models,
                           formulas=fmlas)
  }
  
  if ("svm" %in% panel) {
    if (!require("e1071"))
      stop("'svm' classifier requires 'e1071' package")
    
    # svm
    classif <- cv(x, y, folds=folds,
                  model_callback=function(xtrain, ytrain) {
                    do.call("svm", c(list(x=xtrain, y=ytrain, probability=TRUE), opt$svm))
                  },
                  predict_callback=function(model, xtest, ytest) {
                    prob <- predict(model, newdata=xtest, probability=TRUE)
                    p.df <- attributes(prob)$probabilities
                    p.good <- p.df[,which(colnames(p.df) == class.colname)]
                    prediction(as.vector(p.good), ytest)
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$svm <- list(label="svm",
                            predictions=predictions,
                            truth=truth,
                            models=models)
  }
  
  if ("glm" %in% panel) {
    if (!require("stats"))
      stop("'glm' classifier requires 'stats' package")
    
    # glm
    classif <- cv(x, y, folds=folds,
                  model_callback=function(xtrain, ytrain) {
                    data <- cbind(ytrain, xtrain)
                    colnames(data) <- c("Class", colnames(xtrain))
                    fmla <- as.formula("Class~.")
                    do.call("glm", c(list(formula=fmla, data=data), opt$glm))
                  },
                  predict_callback=function(model, xtest, ytest) {
                    prob <- predict(model, newdata=xtest, type="response")
                    prediction(as.vector(prob), ytest)
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    fmlas <- lapply(classif, function(x) x[["fmla"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$glm <- list(label="glm",
                            predictions=predictions,
                            truth=truth,
                            models=models,
                            formulas=fmlas)
  }
  
  if ("nnet" %in% panel) {
    if (!require("nnet"))
      stop("'nnet' classifier requires 'nnet' package")
    
    # neural network
    classif <- cv(df, fmla, folds=folds,
                  model_callback=function(fmla, data, class) {
                    do.call("nnet", c(list(formula=fmla, data=data), opt$nnet))
                  },
                  predict_callback=function(model, newdata, class) {
                    prob <- predict(model, newdata=newdata, type="raw")
                    prediction(prob[,1], newdata[[class]])
                  },
                  fs_callback=fs, ncores=ncores)
    models <- lapply(classif, function(x) x[["model"]])
    fmlas <- lapply(classif, function(x) x[["fmla"]])
    predictions <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@predictions[[1]])))
    truth <- as.vector(unlist(sapply(classif, function(x) x[["pred"]]@labels[[1]])))
    
    classifiers$nnet <- list(label="nnet",
                             predictions=predictions,
                             truth=truth,
                             models=models,
                             formulas=fmlas)
  }
  
  classifiers
}

#' getFormula
#'
#' @export getFormula
getFormula <- function(col.names, class.idx=1) {
  class.name <- col.names[class.idx]
  as.formula(paste(class.name, " ~ ", paste(col.names[-class.idx], collapse=" + ")))
}