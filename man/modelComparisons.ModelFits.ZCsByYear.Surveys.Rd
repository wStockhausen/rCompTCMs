% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelComparisons.ModelFits.ZCsByYear.Surveys.R
\name{modelComparisons.ModelFits.ZCsByYear.Surveys}
\alias{modelComparisons.ModelFits.ZCsByYear.Surveys}
\title{Render a document of comparison plots for model fits to survey size composition data by year}
\usage{
modelComparisons.ModelFits.ZCsByYear.Surveys(models, plot1stObs = TRUE,
  output_format = c("word_document", "pdf_document"),
  output_dir = getwd(),
  rmd = system.file("rmd/modelComparisons.ModelFits.ZCsByYear.Surveys.Rmd",
  package = "rCompTCMs"),
  docx_styles = system.file("rmd/StylesForRmdDocs.docx", package =
  "wtsUtilities"), pdf_styles = system.file("rmd/StylesForRmdPDFs.sty",
  package = "wtsUtilities"), clean = FALSE)
}
\arguments{
\item{models}{- named list of model results (as resLst objects) to compare}

\item{plot1stObs}{- flag to plot observed data only from first model}

\item{output_format}{- "word_document" or "pdf_document"}

\item{output_dir}{- path to folder to use for output}

\item{rmd}{- Rmd file to process (defalut="rmd/modelComparisons.ModelFits.ZCsByYear.Surveys.Rmd")}

\item{docx_styles}{- full path to Word (docx) style template for Word documents}

\item{pdf_styles}{- full path to style template for pdf documents}

\item{clean}{- T/F to delete intermediate files}

\item{rmd_dir}{- folder enclosing rmd file}
}
\description{
Function to render a document of comparison plots for model fits to
survey size composition data by year.
}
\details{
Resulting document title will be of the form "ModelComparisons.ModelFits.ZCsByYear.Surveys.mmm.ext",
where "ext" is the appropriate file extension and "mmm" is a dash-separated string of model names.
}