% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readParFile.R
\name{readParFile}
\alias{readParFile}
\title{Create a \code{gmacs_par} dataframe from GMACS ADMB par files.}
\usage{
readParFile(parfn = NULL)
}
\arguments{
\item{parfn}{\itemize{
\item a (possibly named) list or vector of par file filenames
}}
}
\value{
a \code{gmacs_par} object (a \link{tibble} dataframe of class 'gmacs_par') corresponding to the par file(s).
}
\description{
Function to create a \code{gmacs_par} dataframe from GMACS ADMB par files.
}
\details{
The returned tibble will have columns
\itemize{
\item{case - model case name (taken from the list or vector element name) or number. Value is 'gmacs' if only an unnamed par file is given.}
\item{item - parameter counter}
\item{name - full parameter name (i.e., 'name\link{xx}'), or 'number of parameters','objective function','max gradient'}
\item{name - parsed parameter name (i.e., 'name'), or 'number of parameters','objective function','max gradient')}
\item{par_idx - parameter number_vector index (i.e., 'xx' as number)}
\item{idx - parameter vector_vector index (i.e., index into parameter vector)}
\item{value - parameter estimate or convergence quantity value}
}
The ADMB convergence information is given in the first three rows of the tibble,
associated with the names 'number of parameters','objective function',and 'max gradient'.

Note that parameters in \code{gmacs_par} and \code{gmacs_std} objects can be linked together by
equating \code{param} and \code{idx} across both the two objects.

Note also that this is not the same format as returned by \code{\link[gmr:readGMACSpar]{gmr::readGMACSpar()}}, which
contains more structured information based on the model run's dat and ctl files.
}
