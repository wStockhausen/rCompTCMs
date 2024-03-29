% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareFits.GrowthData.R
\name{compareFits.GrowthData}
\alias{compareFits.GrowthData}
\title{Compare fits to growth data among several model runs}
\usage{
compareFits.GrowthData(
  objs,
  dodge = 0.2,
  plot1stObs = FALSE,
  pdf = NULL,
  showPlot = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{objs}{\itemize{
\item list of resLst objects
}}

\item{dodge}{\itemize{
\item width to dodge overlapping series
}}

\item{plot1stObs}{\itemize{
\item flag to include observations only from 1st model scenario
}}

\item{pdf}{\itemize{
\item name for output pdf file
}}

\item{showPlot}{\itemize{
\item flag to print plot to current device
}}

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostic information
}}
}
\value{
ggplot object
}
\description{
Function to compare fits to growth data among several model runs.
}
\details{
None.
}
