% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractMDFR.Fisheries.Catchability.R
\name{extractMDFR.Fisheries.Catchability}
\alias{extractMDFR.Fisheries.Catchability}
\title{Function to extract fishery catchabilities by year using ggplot2}
\usage{
extractMDFR.Fisheries.Catchability(
  objs,
  fleets = "all",
  years = "all",
  cast = "x",
  verbose = FALSE
)
}
\arguments{
\item{objs}{\itemize{
\item list of resLst objects
}}

\item{fleets}{\itemize{
\item names of fleets to include (or "all")
}}

\item{years}{\itemize{
\item vector of years to show, or 'all' to show all years
}}

\item{cast}{\itemize{
\item formula to exclude factors from "averaging" over
}}

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostic information
}}
}
\value{
dataframe in canonical format
}
\description{
This function extracts fishery catchability estimates by year,
sex and maturity state.
}
\details{
None.
}
