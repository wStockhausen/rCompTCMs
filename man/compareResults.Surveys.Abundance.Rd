% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareResults.Surveys.Abundance.R
\name{compareResults.Surveys.Abundance}
\alias{compareResults.Surveys.Abundance}
\title{Function to compare estimated survey abundance by year among several models}
\usage{
compareResults.Surveys.Abundance(
  objs,
  fleets = "all",
  cast = "x",
  years = "all",
  facet_grid = NULL,
  facet_wrap = NULL,
  scales = "fixed",
  dodge = 0.2,
  mxy = 15,
  nrow = 5,
  lnscale = FALSE,
  plotPoints = FALSE,
  colour_scale = NULL,
  fill_scale = NULL,
  showPlot = FALSE,
  pdf = NULL,
  verbose = FALSE
)
}
\arguments{
\item{objs}{\itemize{
\item list of resLst objects
}}

\item{fleets}{\itemize{
\item vector of fleets to plot, or "all"
}}

\item{cast}{\itemize{
\item casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
}}

\item{years}{\itemize{
\item 'all' or vector of years to include
}}

\item{facet_grid}{\itemize{
\item formula for faceting using facet_grid
}}

\item{facet_wrap}{\itemize{
\item formula for faceting using facet_wrap
}}

\item{scales}{\itemize{
\item parameter passed to ggplot2::facet_grid()
}}

\item{dodge}{\itemize{
\item width to dodge overlapping series
}}

\item{mxy}{\itemize{
\item max number of years per page
}}

\item{nrow}{\itemize{
\item number of rows per page, when facet_wrap'ing
}}

\item{lnscale}{\itemize{
\item use log scale on y axis
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
\item flag (T/F) to show plot
}}

\item{pdf}{\itemize{
\item creates pdf, if not NULL
}}

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostic information
}}
}
\value{
ggplot2 object
}
\description{
This function compares estimated survey abundance by year
among several models.
}
\details{
If 'z' is a cast'ing factor, then a set of annual size composition plots are produced. Otherwise,
a set of time series plots are produced.
}
