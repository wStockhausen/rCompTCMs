% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelComparisons.Characteristics.Fisheries.R
\name{modelComparisons.Characteristics.Fisheries}
\alias{modelComparisons.Characteristics.Fisheries}
\title{Render a document of comparison plots of estimated fishery characteristics from several models}
\usage{
modelComparisons.Characteristics.Fisheries(
  models,
  fleets = "all",
  selyears = "all",
  retyears = "all",
  output_format = c("word_document", "pdf_document"),
  output_dir = getwd(),
  rmd = system.file("rmd/modelComparisons.EstimatedCharacteristics.Fisheries.Rmd",
    package = "rCompTCMs"),
  docx_styles = system.file("rmd/StylesForRmdDocs.docx", package = "wtsUtilities"),
  pdf_styles = system.file("rmd/StylesForRmdPDFs.sty", package = "wtsUtilities"),
  verbose = FALSE,
  clean = FALSE
)
}
\arguments{
\item{models}{\itemize{
\item named list of model results (as resLst objects) to compare
}}

\item{fleets}{\itemize{
\item vector of fisheries to plot, or "all"
}}

\item{selyears}{\itemize{
\item vector of years to show selectivity functions for, or 'all' to show all years
}}

\item{retyears}{\itemize{
\item vector of years to show retention functions for, or 'all' to show all years
}}

\item{output_format}{\itemize{
\item "word_document" or "pdf_document"
}}

\item{output_dir}{\itemize{
\item path to folder to use for output
}}

\item{rmd}{\itemize{
\item Rmd file to process (defalut=system.file("rmd/modelComparisons.EstimatedCharacteristics.Fisheries.Rmd",package="rCompTCMs"))
}}

\item{docx_styles}{\itemize{
\item full path to Word (docx) style template for Word documents
}}

\item{pdf_styles}{\itemize{
\item full path to style template for pdf documents
}}

\item{verbose}{\itemize{
\item flag to print debugging info
}}

\item{clean}{\itemize{
\item T/F to delete intermediate files
}}

\item{rmd_dir}{\itemize{
\item folder enclosing rmd file
}}
}
\description{
Function to render a document of comparison plots of estimated fishery
characteristics from several models.
}
\details{
The default resulting document title will be of the form "ModelComparisons.EstimatedCharacteristics.Fisheries.mmm.ext",
where "ext" is the appropriate file extension and "mmm" is a dash-separated string of model names.
}
