% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retroPlots.R
\name{retroPlotValuesAsBars}
\alias{retroPlotValuesAsBars}
\title{Create a plot of values from a retrospective analysis as bars}
\usage{
retroPlotValuesAsBars(dfr, xlab = "peel", ylab = "", facet_grid = NULL)
}
\arguments{
\item{dfr}{\itemize{
\item dataframe of estimated values extracted from a series of retrospective model runs
}}

\item{xlab}{\itemize{
\item label for x-axis
}}

\item{ylab}{\itemize{
\item label for y-axis
}}

\item{facet_grid}{\itemize{
\item formula to apply faceting
}}
}
\value{
a ggplot2 object
}
\description{
This function creates a plot of values from a retrospective analysis as bars.
}
\details{
The column "case" should indicate the peels of the retrospective runs
}
