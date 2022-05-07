#'
#'@title Extract all scores from fits to biomass/abundance time series by fleet among several model runs as a dataframe
#'
#'@description Function to extract all scores from biomass/abundance time series by fleet among
#'several model runs.
#'
#' @param objs - object that can be converted into a list of tcsam02.resLst objects
#' @param fleets - names of fleets to include (or "all" or NULL to include all)
#' @param fleet.type - fleet type ('fishery' or 'survey')
#' @param catch.type - catch type ('index','retained',or 'total')
#' @param data.type - 'biomass' or 'abundance'
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses [rTCSAM02::getMDFR.AllScores.Biomass()] or [rTCSAM02::getMDFR.AllScores.Abundance()].
#'
#'@return dataframe in canonical format.
#'
#'@import dplyr
#'@import magrittr
#'@import rTCSAM02
#'
#'@md
#'
#'@export
#'
extractFits.AllScores.ACD<-function(objs=NULL,
                                     fleets="all",
                                     fleet.type=c('survey','fishery'),
                                     catch.type=c('index','retained','total'),
                                     data.type=c('biomass','abundance'),
                                     verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::extractFits.AllScores.ACD().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    data.type <-data.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        #--testing: case=cases[1];
        obj<-objs[[case]];
        if (verbose) message("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam02.resLst")){
          mdfr1 = NULL;
          if (data.type=="biomass"){
            mdfr1<-getMDFR.AllScores.Biomass(obj,
                                          fleet.type=fleet.type,
                                          catch.type=catch.type,
                                          verbose=verbose);
          } else if (data.type=="abundance"){
            mdfr1<-getMDFR.AllScores.Abundance(obj,
                                          fleet.type=fleet.type,
                                          catch.type=catch.type,
                                          verbose=verbose);
          }
          if (!is.null(mdfr1)){
              if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1 %<>% dplyr::filter(fleet %in% fleets);
              mdfr1$case<-case;
              mdfr<-rbind(mdfr,mdfr1);
          }
        }
    }#--case

    if (!is.null(mdfr)){
        mdfr %<>% dplyr::mutate(case=factor(case,levels=cases),
                                x=ifelse(x=='all','all sex',     x),
                                m=ifelse(m=='all','all maturity',m),
                                s=ifelse(s=='all','all shell',   s));
    }

    if (verbose) message("Finished rCompTCMs::extractFits.AllScores.ACD().\n");
    return(mdfr);
}
