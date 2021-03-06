% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pareto.R
\name{pareto}
\alias{pareto}
\title{Approximates Pareto front for multi-objective optimization of AgroEcoTradeoff 
Model outputs}
\usage{
pareto(cnames, step = 0.1, prod_targ, yblist = list(1, 1), currprodmod,
  input_key = "ZA", Yv, Cv, BDv, COSTv, Yst, Cst, BDst, COSTst,
  todisk = FALSE, silent = TRUE, ncl = 4)
}
\arguments{
\item{cnames}{A list of the constraints to optimize over}

\item{step}{The step interval over which to search for optimal solutions}

\item{prod_targ}{Production targets passed as list. See examples for format.}

\item{yblist}{A two element list of yield modifiers (currently disabled)}

\item{currprodmod}{Modifier of current production}

\item{Yv}{agricultural impact metric (new land required)}

\item{Cv}{Carbon impact metric}

\item{BDv}{Biodiversity impact metric}

\item{Yst}{Function to calculate impacts across crops (e.g. sum)}

\item{Cst}{Function to calculate impacts across carbon (e.g. sum)}

\item{BDst}{Function to calculate impacts across biodiversity (e.g. mean)}

\item{COSTst}{Function to calculate cost impact (e.g. mean)}

\item{todisk}{Write out batch results to disk? TRUE or FALSE (default)}

\item{silent}{Verbose simulations?  Doesn't really work with parallel process}

\item{ncl}{Number of cpus to use (default = 4)}

\item{COSTc}{Cost impact metric}
}
\value{
data.table
}
\description{
This function generates a series of possible outputs from the
AgroEcoTradeoff Model and retains those that are non-dominated, effectively
mimicking the traditional weighted sum method for multi-objective optimization.
}

