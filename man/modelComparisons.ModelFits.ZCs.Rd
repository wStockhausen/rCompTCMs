% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelComparisons.ModelFits.ZCs.R
\name{modelComparisons.ModelFits.ZCs}
\alias{modelComparisons.ModelFits.ZCs}
\title{Render a document of comparison plots for model fits to size composition data}
\usage{
modelComparisons.ModelFits.ZCs(
  models,
  type = c("All", "Surveys", "Fisheries"),
  fleets = "all",
  plot1stObs = TRUE,
  output_format = c("word_document", "pdf_document"),
  output_dir = getwd(),
  rmd = NULL,
  docx_styles = system.file("rmd/StylesForRmdDocs.docx", package = "wtsUtilities"),
  pdf_styles = system.file("rmd/StylesForRmdPDFs.sty", package = "wtsUtilities"),
  clean = FALSE
)
}
\arguments{
\item{models}{\itemize{
\item named list of model results (as resLst objects) to compare
}}

\item{type}{\itemize{
\item "All", "Surveys", "Fisheries" (indicating, but not determining,  what types of size compositions will be included)
}}

\item{fleets}{\itemize{
\item vector of fleet names to include (or "all" or NULL for all fleets)
}}

\item{plot1stObs}{\itemize{
\item flag to plot observed data only from first model
}}

\item{output_format}{\itemize{
\item "word_document" or "pdf_document"
}}

\item{output_dir}{\itemize{
\item path to folder to use for output
}}

\item{rmd}{\itemize{
\item Rmd file to process (defalut="modelComparisons.ModelFits.ZCs.Rmd")
}}

\item{docx_styles}{\itemize{
\item full path to Word (docx) style template for Word documents
}}

\item{pdf_styles}{\itemize{
\item full path to style template for pdf documents
}}

\item{clean}{\itemize{
\item T/F to delete intermediate files
}}

\item{rmd_dir}{\itemize{
\item folder enclosing rmd file
}}
}
\description{
Function to render a document of comparison plots for model fits to
size composition data.
}
\details{
Resulting document title will be of the form "ModelComparisons.ModelFits.ZCs.type.mmm.ext",
where "type" is the parameter \code{type}, ext" is the appropriate file extension,
and "mmm" is a dash-separated string of model names. The actual type of size compositions included
is determined by the R Markdown file specified by \code{rmd}.
}
