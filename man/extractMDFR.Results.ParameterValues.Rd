% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractMDFR.Results.ParameterValues.R
\name{extractMDFR.Results.ParameterValues}
\alias{extractMDFR.Results.ParameterValues}
\title{Extract parameter values (and associated std devs) from several model runs as a dataframe}
\usage{
extractMDFR.Results.ParameterValues(objs, verbose = FALSE)
}
\arguments{
\item{objs}{- list of resLst objects}

\item{verbose}{- flag (T/F) to print diagnostic information}
}
\value{
dataframe
}
\description{
This function extracts parameter values (and associated std devs) from several model runs as a dataframe.
}
\details{
Results are extracted using \code{rTCSAM02::getMDFR.Results.ParameterValues} for tcsam02 model runs.
}