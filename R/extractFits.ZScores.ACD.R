#'
#'@title Extract residuals from fits to biomass/abundance time series by fleet among several model runs as a dataframe
#'
#'@description Function to extract residuals from biomass/abundance time series by fleet among
#'several model runs.
#'
#' @param objs - object that can be converted into a list of tcsam02.resLst objects
#' @param fleets - names of fleets to include (or "all" or NULL to include all)
#' @param fleet.type - fleet type ('fishery' or 'survey')
#' @param catch.type - catch type ('index','retained',or 'total')
#' @param data.type - 'biomass' or 'abundance'
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses [rTCSAM02::getMDFR.ZScores.Biomass()] or [rTCSAM02::getMDFR.ZScores.Abundance()].
#'Also uses [wtsUtilities::printGGList()].
#'
#'@return dataframe in canonical format(?).
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
extractFits.ZScores.ACD<-function(objs=NULL,
                                     fleets="all",
                                     fleet.type=c('survey','fishery'),
                                     catch.type=c('index','retained','total'),
                                     data.type=c('biomass','abundance'),
                                     verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::extractFits.ZScores.PrNatZ().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    data.type <-data.type[1];

    if (fleet.type=='survey') catch.type<-'index';
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam02.resLst")){
          mdfr1 = NULL;
          if (data.type=="biomass")
            mdfr1<-getMDFR.ZScores.Biomass(obj,
                                          fleet.type=fleet.type,
                                          catch.type=catch.type,
                                          verbose=verbose);
          if (!is.null(mdfr1)){
              if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1 %<>% dplyr::filter(fleet %in% fleets);
              mdfr1$case<-case;
              mdfr<-rbind(mdfr,mdfr1);
          }
        }
    }#--case

    mdfr %<>% dplyr::mutate(case=factor(case,levels=cases),
                            x=ifelse(x=='all','all sex',     x),
                            m=ifelse(m=='all','all maturity',m),
                            s=ifelse(s=='all','all shell',   s));

    if (verbose) cat("Finished rCompTCMs::extractFits.ZScores.ACD().\n");
    return(mdfr);
}
