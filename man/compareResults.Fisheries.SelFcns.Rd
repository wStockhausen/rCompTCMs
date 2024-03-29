% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareResults.Fisheries.SelFcns.R
\name{compareResults.Fisheries.SelFcns}
\alias{compareResults.Fisheries.SelFcns}
\title{Function to compare fishery selectivity functions by year among several models}
\usage{
compareResults.Fisheries.SelFcns(
  objs,
  cast = "y+x",
  fleets = "all",
  years = "all",
  dodge = 0.2,
  singlePlot = FALSE,
  mxy = 15,
  facet_wrap = NULL,
  facet_grid = ifelse(singlePlot, "x~case", "y~x"),
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

\item{cast}{\itemize{
\item formula to exclude factors from "averaging" over
}}

\item{fleets}{\itemize{
\item vector of fleets to plot, or "all"
}}

\item{years}{\itemize{
\item vector of years to show, or 'all' to show all years
}}

\item{dodge}{\itemize{
\item width to dodge overlapping series
}}

\item{singlePlot}{\itemize{
\item flag to plot all years on single plot (be sure to adjust facet_grid)
}}

\item{mxy}{\itemize{
\item max number of years per page
}}

\item{facet_wrap}{\itemize{
\item ggplot2 formula to produce figure with wrapped facets
}}

\item{facet_grid}{\itemize{
\item ggplot2 formula to produce figure with gridded facets
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
lists ofggplot2 objects, nested by fishery or an empty list if year is NULL
}
\description{
This function compares fishery selectivity functions by year
among several models.
}
\details{
None.
}
