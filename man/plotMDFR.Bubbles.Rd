% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMDFR.Bubbles.R
\name{plotMDFR.Bubbles}
\alias{plotMDFR.Bubbles}
\title{Create or add to a circle plot of age or size comp.s.}
\usage{
plotMDFR.Bubbles(
  mdfr,
  x = NULL,
  y = NULL,
  value.var = "val",
  agg.formula = NULL,
  agg.function = sum,
  ...,
  colour = NULL,
  facet_grid = NULL,
  units = "",
  xlab = "",
  ylab = "",
  title = "",
  alpha = 0.5,
  maxBubbleSize = 6,
  useColourGradient = FALSE,
  guideTitleColour = "",
  showPlot = FALSE
)
}
\arguments{
\item{x}{\itemize{
\item column name for x-axis values
}}

\item{y}{\itemize{
\item column name for y-axis values
}}

\item{value.var}{\itemize{
\item column name for values to aggregate (value.var in cast)/plot as circles
}}

\item{agg.formula}{\itemize{
\item aggregation formula (left-hand side of cast formula)
}}

\item{agg.function}{\itemize{
\item aggregation function (fun.aggregate in cast)
}}

\item{...}{\itemize{
\item further arguments passed to aggregating function
}}

\item{colour}{\itemize{
\item column name to which colour aesthetic is mapped
}}

\item{facet_grid}{\itemize{
\item faceting formula
}}

\item{units}{\itemize{
\item units for bubble size scale
}}

\item{xlab}{\itemize{
\item x axis label
}}

\item{ylab}{\itemize{
\item y axis label
}}

\item{title}{\itemize{
\item plot title
}}

\item{alpha}{\itemize{
\item transparency level
}}

\item{maxBubbleSize}{\itemize{
\item max bubble size
}}

\item{useColourGradient}{\itemize{
\item flag (T/F) to use a color gradient for bubble color
}}

\item{guideTitleColour}{\itemize{
\item title for colour guide
}}

\item{showPlot}{\itemize{
\item flag to show plot immediately
}}
}
\value{
ggplot2 object
}
\description{
Creates or adds to a circle plot of age or size comp.s.
}
\details{
uses \code{reshape2} package.
}
