% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch_params.R
\name{batch_params}
\alias{batch_params}
\title{Create parameter table for batch run}
\usage{
batch_params(yblist = list(c(1, 1)), targlist, cblist, currprodmodlist)
}
\arguments{
\item{yblist}{List of yield modification parameters (option switched off now)}

\item{targlist}{List of crop target vectors}

\item{cblist}{List of weight parameters}

\item{currprodmodlist}{List of current production-level modifiers}
}
\value{
data.frame of input parameters
}
\description{
Create parameter table for batch run
}
\details{
For use in pareto and tradeoff_batch
}
\keyword{internal}

