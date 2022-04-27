#'
#' @title Render a document of model comparison plots for various management quantities
#'
#' @description Function to render a document of model comparison plots for various management quantities
#'
#' @param models - named list of model results (as resLst objects) to compare
#' @param base - name or index of base case for difference calculations
#' @param output_format - "word_document" or "pdf_document"
#' @param output_dir - path to folder to use for output
#' @param rmd - Rmd file to process (defalut=system.file("rmd/modelComparisons.Rmd",package="rCompTCMs"))
#' @param docx_styles - full path to Word (docx) style template for Word documents
#' @param pdf_styles - full path to style template for pdf documents
#' @param verbose - flag (T/F) to print diagnostic output
#' @param clean - T/F to delete intermediate files
#'
#' @details Resulting document title will be of the form "ModelComparisons.ManagementQuantities.mmm.ext", where "ext" is the appropriate
#' file extension and "mmm" is a dash-separated string of model names.
#'
#' @export
#'
modelComparisons<-function(models,
                           base=1,
                           output_format=c("word_document","pdf_document"),
                           output_dir=getwd(),
                           rmd=system.file("rmd/modelComparisons.Rmd",package="rCompTCMs"),
                           docx_styles=system.file("rmd/StylesForRmdDocs.docx",package="wtsUtilities"),
                           pdf_styles=system.file("rmd/StylesForRmdPDFs.sty",package="wtsUtilities"),
                           verbose=FALSE,
                           clean=FALSE){
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
  output_file<-paste0("ModelComparisons.ManagementQuantities",mmm,".",ext);
  title<-paste0("Model Comparisons: Management Quantities -- ",mmv);
  cat("Rendering to '",file.path(output_dir,output_file),"'\n",sep="")
  cat("Title: '",title,"'\n",sep='')
  cat("Base RMD folder \n\t'",bsf,"'\n",sep="");

  rmarkdown::render(rmd,
                     output_format=output_format,
                     output_file=output_file,
                     output_dir=output_dir,
                     intermediates_dir=output_dir,
                     output_options=output_options,
                     params=list(title=title,Models=models,doc_type=doc_type,
                                 ModelQuantities=list(base=base,
                                                      verbose=verbose)),
                     clean=clean);
  #res<-file.remove("modelComparisons.knit.md","modelComparisons.utf8.md");
}
