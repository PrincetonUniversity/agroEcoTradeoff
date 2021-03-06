% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targets.R
\name{targets}
\alias{targets}
\title{Calculates crop production targets}
\usage{
targets(prod_targ, currprod, currprodmod)
}
\arguments{
\item{prod_targ}{A named list given the production targets for each crop as a multiple (greater than 1)}

\item{currprod}{A data.table of the current production coming from existing croplands}

\item{currprodmod}{A vector to modify production levels on existing cropland.}
}
\description{
This module calculates the amount of production (in tonnes) for each modelled crop, given
user-specified targets, how much of it can be met on existing cropland, and how much needs to come from 
new cropland. This version works with dta.tables.
}
\details{
This determines how much cropland has to be converted. The argument 
currprodmod can be used to simulate an assumption that yield gaps on current
cropland will be closed before new land is needed (or that a greater amount of 
new land will be needed if current production falls, if currprodmod < 1).
}
\examples{
il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
currprodmod <- 1
currprod <- il$currprod
prod_targ <- c("maize" = 2, "soy" = 2)
targets(prod_targ, currprod, currprodmod)
currprodmod <- c(1.2, 0.9)
targets(prod_targ, currprod, currprodmod)
currprodmod <- c(1.2, 0.9, 0.8)
targets(prod_targ, currprod, currprodmod)  # fails
currprodmod <- NULL
targets(prod_targ, currprod, currprodmod)  # fails
currprodmod <- c(1, 1)
targets(prod_targ, currprod, currprodmod)  # fails
currprodmod <- c(2, 2)
targets(prod_targ, currprod, currprodmod)  # fails
currprodmod <- c(0.5, 0.5)
targets(prod_targ, currprod, currprodmod)
}

