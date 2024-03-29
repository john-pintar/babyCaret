% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preProc.R
\name{matchProc}
\alias{matchProc}
\title{Preprocess new data}
\usage{
matchProc(x, newData, na.rm = FALSE, ...)
}
\arguments{
\item{x}{Either a data.frame returned by preProc, or trainedModel object returned by train()}

\item{newData}{The dataset to be processed.}

\item{na.rm}{A logical. If TRUE, rows containing NA are dropped.}
}
\value{
A data.frame processed by the same metrics used to process x
}
\description{
Preprocesses a new dataset using the metrics used already used to preprocess
A dataset.
}
\details{
Relies on attributed assigned to x by preProc()
}
