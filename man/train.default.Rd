% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train.R
\name{train.default}
\alias{train.default}
\title{ML training from target-vector}
\usage{
(x, data, method, tune)
}
\arguments{
\item{x}{A target-vector. Must be contained in data.}

\item{method}{A string indicating the ML algorithm to be used. See generic function.}

\item{tune}{A model-specific tune object. See generic function.}

\item{data}{A data.frame containing the entire training set.}
}
\value{
A trained ML model.
}
\description{
Trains a machine learning model using target-vector & data.frame input.
}
\details{
If used for clustering or rule mining, the model frame created from data
will be used for training
}
