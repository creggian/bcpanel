% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{auc}
\alias{auc}
\title{it returns a dataframe with auc performances}
\usage{
auc(classifiers, labels = NULL)
}
\arguments{
\item{classifiers}{a list, where each element contains three information
i) 'label': string representing the classifier used
ii) 'predictions': vector of numerical prediction
iii) 'truth': vector of true results}
}
\description{
it returns a dataframe with auc performances
}
