% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareResults.Pop.RecSizeDistribution.R
\name{compareResults.Pop.RecSizeDistribution}
\alias{compareResults.Pop.RecSizeDistribution}
\title{Compare recruitment size distributions among several model runs}
\usage{
compareResults.Pop.RecSizeDistribution(
  objs,
  scaleToDensity = FALSE,
  aggToCutpts = FALSE,
  cutpts = seq(25, 185, 5),
  dodge = 0.2,
  plotPoints = FALSE,
  colour_scale = NULL,
  fill_scale = NULL,
  showPlot = TRUE,
  pdf = NULL,
  verbose = FALSE
)
}
\arguments{
\item{objs}{\itemize{
\item list of resLst objects (or a dataframe from \code{\link[=extractMDFR.Pop.RecSizeDistribution]{extractMDFR.Pop.RecSizeDistribution()}})
}}

\item{scaleToDensity}{\itemize{
\item flag to scale abundance to 1-mm size bins
}}

\item{aggToCutpts}{\itemize{
\item flag to aggregate (rebin) to provided cutpts
}}

\item{cutpts}{\itemize{
\item cutpoints to aggregate to
}}

\item{dodge}{\itemize{
\item width to dodge overlapping series
}}

\item{plotPoints}{\itemize{
\item flag to include points (default: FALSE)
}}

\item{colour_scale}{\itemize{
\item ggplot2 colour scale to substitute for default (if not NULL)
}}

\item{fill_scale}{\itemize{
\item ggplot2 fill scale to substitute for default (if not NULL)
}}

\item{showPlot}{\itemize{
\item flag to print plot to current device
}}

\item{pdf}{\itemize{
\item name for output pdf file
}}

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostic information
}}
}
\value{
ggplot object
}
\description{
Function to compare recruitment size distributions among several model runs.
}
\details{
If scaleToDensity is true, the size distribution is scaled to abundance/mm to allow easier comparison between models
with different bin sizes. If aggToCutpts is true, the distribution(s) are re-binned (aggregated) to a common set of
cutpoints. If objs is a list of resLst objects, then \code{\link[=extractMDFR.Pop.RecSizeDistribution]{extractMDFR.Pop.RecSizeDistribution()}} is called first to obtain the size
distributions.
}
