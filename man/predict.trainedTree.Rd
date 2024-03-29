% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.trainedTree}
\alias{predict.trainedTree}
\title{Predict from tree models.}
\usage{
\method{predict}{trainedTree}(trainedModel, newData, dfOut = TRUE)
}
\arguments{
\item{trainedModel}{A trainedTree object created by train()}

\item{newData}{A data.frame with missing target values.}

\item{dfOut}{A logical. If TRUE, the predictions will be returned as a complete data.frame. If FALSE,
the predictions will be returned as a vector.}
}
\value{
Predictions for newData's target values represented as either a data.frame or vector.
}
\description{
Predicts values using a trainedTree model.
}
\details{
newData should be formatted identical to the model's training set. Column names
and order should be identical. All columns present in training must be included. The number of rows may differ.
Also, prediction will work even if newData's target values are not missing.
}
