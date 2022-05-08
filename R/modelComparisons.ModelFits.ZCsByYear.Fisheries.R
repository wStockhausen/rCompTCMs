#'
#' @title Render a document of comparison plots for model fits to fishery size composition data by year
#'
#' @description Function to render a document of comparison plots for model fits to
#' fishery size composition data by year.
#'
#' @param models - named list of model results (as resLst objects) to compare
#' @param fleets - names of fleets to include (or "all")
#' @param years - years to plot, as numerical vector (or "all" to plot all years)
#' @param plot1stObs - flag (T/F) to plot observations only from first case, or character vector cases cases from which to plot observations
#' @param nrow - number of rows per page for output plots
#' @param ncol - number of columns per page for output plots
#' @param useBars - flag to use bars for observations
#' @param usePins - flag to use pins for observations
#' @param usePinsAndPts - flag to add pts to observations when pins are used
#' @param useLines - flag to use lines for predictions
#' @param usePoints - flag to use points for predictions
#' @param lineSize - prediction line size
#' @param pointSize - prediction point size
#' @param alpha - prediction transparency
#' @param stripText - [ggplot2::element_text()] object describing font and margin to use for panel strips
#' @param output_format - "word_document" or "pdf_document"
#' @param output_dir - path to folder to use for output
#' @param rmd_dir - folder enclosing rmd file
#' @param rmd - Rmd file to process (defalut="rmd/modelComparisons.ModelFits.ZCsByYear.Fisheries.Rmd")
#' @param docx_styles - full path to Word (docx) style template for Word documents
#' @param pdf_styles - full path to style template for pdf documents
#' @param clean - T/F to delete intermediate files
#'
#' @details Resulting document title will be of the form "ModelComparisons.ModelFits.ZCsByYear.Fisheries.mmm.ext",
#' where "ext" is the appropriate file extension and "mmm" is a dash-separated string of model names.
#'
#' @export
#'
modelComparisons.ModelFits.ZCsByYear.Fisheries<-function(
        models,
        fleets="all",
        years='all',
        plot1stObs=TRUE,
        nrow=5,
        ncol=4,
        useBars=TRUE,
        usePins=FALSE,
        usePinsAndPts=FALSE,
        useLines=TRUE,
        usePoints=TRUE,
        lineSize=1,
        pointSize=1,
        alpha=0.5,
        stripText=ggplot2::element_text(),
        output_format=c("word_document","pdf_document"),
        output_dir=getwd(),
        rmd=system.file("rmd/modelComparisons.ModelFits.ZCsByYear.Fisheries.Rmd",package="rCompTCMs"),
        docx_styles=system.file("rmd/StylesForRmdDocs.docx",package="wtsUtilities"),
        pdf_styles=system.file("rmd/StylesForRmdPDFs.sty",package="wtsUtilities"),
        clean=FALSE
    ){
  nms<-names(models);
  mmm<-paste0(nms,collapse="-");
  mmv<-paste0(nms,collapse=" vs ");
  output_format<-output_format[1];
  output_options<-NULL;

  #get base folder enclosing rmd file
  rmd<-normalizePath(rmd);
  bsf<-dirname(rmd);

  if(output_format=="word_document") {
    doc_type<-"word";
    ext<-"docx";
    output_options<-list(reference_docx=docx_styles);
  } else if(output_format=="pdf_document")  {
    doc_type<-"pdf";
    ext<-"pdf";
    output_options<-list(includes=list(in_header=pdf_styles));
  }
  output_file<-paste0("ModelComparisons.ModelFits.ZCsByYear.Fisheries.",mmm,".",ext);
  title<-paste0("Model Comparisons: Fits to Fishery Size Composition Data -- ",mmv);
  cat("Rendering to '",file.path(output_dir,output_file),"'\n",sep="")
  cat("Title: '",title,"'\n",sep='')
  cat("Base RMD folder \n\t'",bsf,"'\n",sep="");

  rmarkdown::render(
            rmd,
            output_format=output_format,
            output_file=output_file,
            output_dir=output_dir,
            intermediates_dir=output_dir,
            output_options=output_options,
            params=list(title=title,
                        Models=models,
                        fleets=fleets,
                        years=years,
                        plot1stObs=plot1stObs,
                        nrow=nrow,
                        ncol=ncol,
                        useBars=useBars,
                        usePins=usePins,
                        usePinsAndPts=usePinsAndPts,
                        useLines=useLines,
                        usePoints=usePoints,
                        lineSize=lineSize,
                        pointSize=pointSize,
                        alpha=alpha,
                        stripText=stripText,
                        doc_type=doc_type),
            clean=clean);
}

