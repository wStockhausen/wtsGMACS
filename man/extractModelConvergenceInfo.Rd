% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractModelConvergenceInfo.R
\name{extractModelConvergenceInfo}
\alias{extractModelConvergenceInfo}
\title{Extract model convergence info}
\usage{
extractModelConvergenceInfo(obj)
}
\arguments{
\item{obj}{\itemize{
\item the object to extract the info from
}}
}
\value{
dataframe
}
\description{
Function to extract model configuration info from a
gmacs_reslst or gmacs_par object.
}
\details{
The returned dataframe has convergence info by model 'case'.
}
