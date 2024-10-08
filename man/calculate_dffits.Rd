% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_dffits.R
\name{calculate_dffits}
\alias{calculate_dffits}
\title{Calculate DFFITS Measure}
\usage{
calculate_dffits(X, y)
}
\arguments{
\item{X}{A numeric matrix representing the design matrix (including the intercept term).}

\item{y}{A numeric vector representing the response variable.}
}
\value{
A numeric vector containing DFFITS for each observation.
}
\description{
This function calculates DFFITS for each observation in the dataset, which
is a measure of the influence of each observation on the fitted regression model.
}
\keyword{internal}
