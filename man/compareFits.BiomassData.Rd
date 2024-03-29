% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareFits.BiomassData.R
\name{compareFits.BiomassData}
\alias{compareFits.BiomassData}
\title{Compare fits to biomass time series by fleet among several model runs}
\usage{
compareFits.BiomassData(
  objs = NULL,
  fleets = "all",
  fleet.type = c("survey", "fishery"),
  catch.type = c("index", "retained", "discard", "total"),
  ci = 0.8,
  fishery.pdfType = c("norm2", "normal", "lognormal"),
  numRecent = 15,
  plot1stObs = TRUE,
  scales = "free_y",
  position = position_dodge(0.3),
  pdf = NULL,
  showPlot = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{objs}{\itemize{
\item object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects, OR a melted dataframe
}}

\item{fleets}{\itemize{
\item names of fleets to include (or "all")
}}

\item{fleet.type}{\itemize{
\item fleet type ('fishery' or 'survey')
}}

\item{catch.type}{\itemize{
\item catch type ('index','retained',  or 'total')
}}

\item{ci}{\itemize{
\item confidence interval for plots
}}

\item{fishery.pdfType}{\itemize{
\item choice for fishery CIs ("norm2","normal","lognormal")
}}

\item{numRecent}{\itemize{
\item number of recent years to include in "recent years" plots
}}

\item{plot1stObs}{\itemize{
\item flag (T/F) to plot observations only from the first model
}}

\item{scales}{\itemize{
\item ggplot2 scales option for facet_grid
}}

\item{position}{\itemize{
\item indicates ggplot2 position_ to use ('dodge','jitter','identity',)
}}

\item{pdf}{\itemize{
\item name for output pdf file
}}

\item{showPlot}{\itemize{
\item flag (T/F) to show plot
}}

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostic information
}}
}
\value{
Non-nested list of ggplot2 objects, with captions as names, or NULL.
}
\description{
Function to compare fits to biomass time series by fleet among
several model runs.
}
\details{
If \code{objs} is a list, then \code{\link[=extractMDFR.Fits.BiomassData]{extractMDFR.Fits.BiomassData()}} is used to extract a melted dataframe
in canonical format with the biomass observations and predictions to be plotted. \code{objs} can also be a melted
dataframe from a previous call to \code{\link[=extractMDFR.Fits.BiomassData]{extractMDFR.Fits.BiomassData()}}.
Also uses \code{\link[wtsUtilities:printGGList]{wtsUtilities::printGGList()}}.
}
