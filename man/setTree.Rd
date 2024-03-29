% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setTune.R
\name{setTree}
\alias{setTree}
\title{Create tuneTree object.}
\usage{
tuneTree(maxDepth = 30, minSplit = 2)
}
\arguments{
\item{maxDepth}{An integer between 1 and 30. Determines the maximum depth of the tree.}

\item{minSplit}{An non-negative integer. Sets the number of instances a node can contain before it is no longer a candidate for further splitting.}
}
\value{
A tuneTree object.
}
\description{
Creates an object which holds user parameters for the Decision Tree algorithm.
}
\seealso{
rpart
}
