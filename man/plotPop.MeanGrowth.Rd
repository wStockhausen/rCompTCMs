% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPop.MeanGrowth.R
\name{plotPop.MeanGrowth}
\alias{plotPop.MeanGrowth}
\title{Plot mean post-molt size for several model runs}
\usage{
plotPop.MeanGrowth(
  mdfr,
  dodge = 0.2,
  plotPoints = FALSE,
  colour_scale = NULL,
  fill_scale = NULL,
  showPlot = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{mdfr}{\itemize{
\item melted dataframe
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

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostic information
}}
}
\value{
ggplot object
}
\description{
Function to plot mean post-molt size for several model runs.
}
\details{
None.
}
