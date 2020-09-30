#'
#' @title Render a document of comparison plots for model fits to size composition data
#'
#' @description Function to render a document of comparison plots for model fits to
#' size composition data.
#'
#' @param models - named list of model results (as resLst objects) to compare
#' @param type - "All", "Surveys", "Fisheries" (indicating, but not determining,  what types of size compositions will be included)
#' @param fleets - vector of fleet names to include (or "all" or NULL for all fleets)
#' @param plot1stObs - flag to plot observed data only from first model
#' @param output_format - "word_document" or "pdf_document"
#' @param output_dir - path to folder to use for output
#' @param rmd_dir - folder enclosing rmd file
#' @param rmd - Rmd file to process (defalut="modelComparisons.ModelFits.ZCs.Rmd")
#' @param docx_styles - full path to Word (docx) style template for Word documents
#' @param pdf_styles - full path to style template for pdf documents
#' @param clean - T/F to delete intermediate files
#'
#' @details Resulting document title will be of the form "ModelComparisons.ModelFits.ZCs.type.mmm.ext",
#' where "type" is the parameter \code{type}, ext" is the appropriate file extension,
#' and "mmm" is a dash-separated string of model names. The actual type of size compositions included
#' is determined by the R Markdown file specified by \code{rmd}.
#'
#' @export
#'
modelComparisons.ModelFits.ZCs<-function(models,
                                         type=c("All","Surveys","Fisheries"),
                                         fleets="all",
                                         plot1stObs=TRUE,
                                         output_format=c("word_document","pdf_document"),
                                         output_dir=getwd(),
                                         rmd=NULL,
                                         docx_styles=system.file("rmd/StylesForRmdDocs.docx",package="wtsUtilities"),
                                         pdf_styles=system.file("rmd/StylesForRmdPDFs.sty",package="wtsUtilities"),
                                         clean=FALSE){
  if (is.null(rmd)){
      #default types
      types<-c("all","surveys","fisheries");
      #default RMDs
      rmds <- c(system.file("rmd/modelComparisons.ModelFits.ZCs.All.Rmd",package="rCompTCMs"),
                system.file("rmd/modelComparisons.ModelFits.ZCs.Surveys.Rmd",package="rCompTCMs"),
                system.file("rmd/modelComparisons.ModelFits.ZCs.Fisheries.Rmd",package="rCompTCMs"));
      rmd<-rmds[types == tolower(type[1])];
  }

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
  output_file<-paste0("ModelComparisons.ModelFits.ZCs.",type,".",mmm,".",ext);
  title<-paste0("Model Comparisons: Fits to ",type," Size Composition Data -- ",mmv);
  cat("Rendering to '",file.path(output_dir,output_file),"'\n",sep="")
  cat("Title: '",title,"'\n",sep='')
  cat("Base RMD folder \n\t'",bsf,"'\n",sep="");

  rmarkdown::render(rmd,
                     output_format=output_format,
                     output_file=output_file,
                     output_dir=output_dir,
                     intermediates_dir=output_dir,
                     output_options=output_options,
                     params=list(title=title,Models=models,fleets=fleets,plot1stObs=plot1stObs,doc_type=doc_type),
                     clean=clean);
}

