% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotZScores.R
\name{plotZScores}
\alias{plotZScores}
\title{Plot z-scores from a set of model runs}
\usage{
plotZScores(
  dfr,
  x = "y",
  y = "zscore",
  color = NULL,
  shape = NULL,
  size = 2,
  position = "identity",
  dodge = 0.2,
  facets = NULL,
  facet.scales = "fixed",
  xlab = "year",
  ylab = NULL,
  title = NULL,
  legend = NULL,
  xlims = NULL,
  ylims = NULL,
  alpha = 1,
  plotPoints = TRUE,
  colour_scale = ggplot2::scale_colour_hue(),
  fill_scale = ggplot2::scale_fill_hue(),
  showSmooths = TRUE,
  showPlot = FALSE
)
}
\arguments{
\item{dfr}{\itemize{
\item dataframe
}}

\item{x}{\itemize{
\item column name for x axis (default = 'year')
}}

\item{y}{\itemize{
\item column name for y axis (default='z-score')
}}

\item{color}{\itemize{
\item column name for color levels (or NULL)
}}

\item{shape}{\itemize{
\item column name for shape levels (or NULL)
}}

\item{size}{\itemize{
\item size for shapes
}}

\item{position}{\itemize{
\item indicates ggplot2 position_ to use ('dodge','jitter','identity',)
}}

\item{dodge}{\itemize{
\item value to position dodge
}}

\item{facets}{\itemize{
\item string giving faceting formula for facet_grid
}}

\item{facet.scales}{\itemize{
\item ggplot2 scales option for facet_grid
}}

\item{xlab}{\itemize{
\item label for x axis
}}

\item{ylab}{\itemize{
\item label for y axis
}}

\item{title}{\itemize{
\item title for plot
}}

\item{legend}{\itemize{
\item legend title
}}

\item{xlims}{\itemize{
\item limits for x axis
}}

\item{ylims}{\itemize{
\item limits for y axis
}}

\item{alpha}{\itemize{
\item transparency value to use
}}

\item{plotPoints}{\itemize{
\item flag to include points on plots
}}

\item{colour_scale}{\itemize{
\item ggplot2 scale_colour object (default is \code{\link[ggplot2:scale_hue]{ggplot2::scale_colour_hue()}})
}}

\item{fill_scale}{\itemize{
\item ggplot2 scale_fill object (default is \code{\link[ggplot2:scale_hue]{ggplot2::scale_fill_hue()}})
}}

\item{showSmooths}{\itemize{
\item flag (T/F) to show smooth fits to z-scores
}}

\item{showPlot}{\itemize{
\item flag (T/F) to show plot immediately
}}
}
\value{
ggplot2 object
}
\description{
Function to plot z-scores from a set of model runs.
}
\details{
Smooth fits are grouped by 'color'.
}
