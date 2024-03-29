#'
#' @title Render a document of model comparison plots
#'
#' @description Function to render a document of model comparison plots
#'
#' @param models - named list of model results (as resLst objects) to compare
#' @param includeInputData - flag (T/F) to include section
#' @param InputData - list controlling what gets plotted
#' @param includeManagementQuantities - flag to include comparison of management quantities
#' @param ManagementQuantities - list controlling comparison of management quantities (see [modelComparisons.ManagementQuantities()])
#' @param includeParameterTables - flag (T/F) to include section
#' @param ParameterTables - list(verbose=FALSE)  (see [modelComparisons.ParameterTables()])
#' @param includePopProcesses - flag (T/F) to include section
#' @param PopProcesses - list(verbose=FALSE) (see [modelComparisons.PopProcesses()])
#' @param includePopQuantities - flag (T/F) to include section
#' @param PopQuantities - list(verbose=FALSE)  (see [modelComparisons.PopQuantitiess()])
#' @param includeModelFitsToOtherData - flag (T/F) to include section
#' @param ModelFitsToOtherData - list(plot1stObs=TRUE,minSizeForMaturityData=60,verbose=FALSE)  (see [modelComparisons.ModelFitsToOtherData()])
#' @param includeModelFitsACD - flag (T/F) to include section
#' @param ModelFitsACD - list(plot1stObs=TRUE,numRecent=30,verbose=FALSE) (see [modelComparisons.ModelFitsACD()])
#' @param includeModelFitsZCs - flag (T/F) to include section
#' @param ModelFitsZCs - list(plot1stObs=TRUE,fleets="all",verbose=FALSE) (see [modelComparisons.ModelFitsZCs()])
#' @param includeCharacteristicsForSurveys - flag (T/F) to include section
#' @param CharacteristicsForSurveys - list(plot1stObs=TRUE,fleets="all",selyears="all",avlyears="all",capyears="all",mxy=5,verbose=FALSE)  (see [modelComparisons.CharacteristicsSurveys()])
#' @param includeCharacteristicsForFisheries - flag (T/F) to include section
#' @param CharacteristicsForFisheries - list(plot1stObs=TRUE,fleets="all",selyears="all",retyears="all",verbose=FALSE) (see [modelComparisons.CharacteristicsFisheries()])
#' @param output_format - "word_document" or "pdf_document"
#' @param output_dir - path to folder to use for output
#' @param rmd - Rmd file to process (defalut=system.file("rmd/modelComparisons.Rmd",package="rCompTCMs"))
#' @param docx_styles - full path to Word (docx) style template for Word documents
#' @param pdf_styles - full path to style template for pdf documents
#' @param clean - T/F to delete intermediate files
#'
#' @details Resulting document title will be of the form "ModelComparisons.mmm.ext", where "ext" is the appropriate
#' file extension and "mmm" is a dash-separated string of model names.
#'
#'@md
#'
#' @export
#'
modelComparisons<-function(models,
                           includeManagementQuantities=FALSE,
                           ManagementQuantities=list(base=1,verbose=FALSE),
                            includeInputData=FALSE,
                            InputData=list(includeSurveyData=TRUE,
                                            surveys=list(plotAbundance=TRUE,
                                                         plotBiomass=TRUE,
                                                         plotZCs=TRUE,
                                                         fleets="all",
                                                         ci=0.80,
                                                         numRecent=30),
                                            includeFisheryData=TRUE,
                                            fisheries=list(fleets="all",
                                                           retained=list(plotAbundance=TRUE,
                                                                         plotBiomass=TRUE,
                                                                         plotZCs=TRUE,
                                                                         ci=0.80,
                                                                         numRecent=30),
                                                           total=list(plotAbundance=TRUE,
                                                                         plotBiomass=TRUE,
                                                                         plotZCs=TRUE,
                                                                         ci=0.80,
                                                                         numRecent=30),
                                                           effort=list(plot=TRUE,
                                                                       numRecent=30)),
                                            includeGrowthData=TRUE,
                                            includeMaturityOgiveData=TRUE,
                                           verbose=FALSE),
                            includeParameterTables=FALSE,
                            ParameterTables=list(categories="all",
                                                 processes="all",
                                                 types="all",
                                                 params="all",
                                                 verbose=FALSE),
                            includePopProcesses=FALSE,
                            PopProcesses=list(verbose=FALSE),
                            includePopQuantities=FALSE,
                            PopQuantities=list(verbose=FALSE),
                            includeModelFitsToOtherData=FALSE,
                            ModelFitsToOtherData=list(plot1stObs=TRUE,
                                                      minSizeForMaturityData=60,
                                                      verbose=FALSE),
                            includeModelFitsACD=FALSE,
                            ModelFitsACD=list(plot1stObs=TRUE,
                                              numRecent=30,
                                              verbose=FALSE),
                            includeModelFitsZCs=FALSE,
                            ModelFitsZCs=list(plot1stObs=TRUE,
                                              fleets="all",
                                              verbose=FALSE),
                            includeCharacteristicsForSurveys=FALSE,
                            CharacteristicsForSurveys=list(plot1stObs=TRUE,
                                                       fleets="all",
                                                       selyears="all",
                                                       avlyears="all",
                                                       capyears="all",
                                                       mxy=5,
                                                       verbose=FALSE),
                            includeCharacteristicsForFisheries=FALSE,
                            CharacteristicsForFisheries=list(plot1stObs=TRUE,
                                                         fleets="all",
                                                         selyears="all",
                                                         retyears="all",
                                                         verbose=FALSE),
                            output_format=c("word_document","pdf_document"),
                            output_dir=getwd(),
                            rmd=system.file("rmd/modelComparisons.Rmd",package="rCompTCMs"),
                            docx_styles=system.file("rmd/StylesForRmdDocs.docx",package="wtsUtilities"),
                            pdf_styles=system.file("rmd/StylesForRmdPDFs.sty",package="wtsUtilities"),
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
  output_file<-paste0("ModelComparisons.",mmm,".",ext);
  title<-paste0("Model Comparisons for ",mmv);#--NOTE: have to double-escape here?
  cat("Rendering to '",file.path(output_dir,output_file),"'\n",sep="")
  cat("Title: '",title,"'\n",sep='')
  cat("Base RMD folder \n\t'",bsf,"'\n",sep="");

  rmarkdown::render(rmd,
                     output_format=output_format,
                     output_file=output_file,
                     output_dir=output_dir,
                     intermediates_dir=output_dir,
                     output_options=output_options,
                     params=list(Models=models,title=title,doc_type=doc_type,
                                 includeManagementQuantities=includeManagementQuantities,
                                 ManagementQuantities=ManagementQuantities,
                                 includeInputData=includeInputData,
                                 InputData=InputData,
                                 includeParameterTables=includeParameterTables,
                                 ParameterTables=ParameterTables,
                                 includePopProcesses=includePopProcesses,
                                 PopProcesses=PopProcesses,
                                 includePopQuantities=includePopQuantities,
                                 PopQuantities=PopQuantities,
                                 includeModelFitsToOtherData=includeModelFitsToOtherData,
                                 ModelFitsToOtherData=ModelFitsToOtherData,
                                 includeModelFitsACD=includeModelFitsACD,
                                 ModelFitsACD=ModelFitsACD,
                                 includeModelFitsZCs=includeModelFitsZCs,
                                 ModelFitsZCs=ModelFitsZCs,
                                 includeCharacteristicsForSurveys=includeCharacteristicsForSurveys,
                                 CharacteristicsForSurveys=CharacteristicsForSurveys,
                                 includeCharacteristicsForFisheries=includeCharacteristicsForFisheries,
                                 CharacteristicsForFisheries=CharacteristicsForFisheries),
                     clean=clean);
  #res<-file.remove("modelComparisons.knit.md","modelComparisons.utf8.md");
}

