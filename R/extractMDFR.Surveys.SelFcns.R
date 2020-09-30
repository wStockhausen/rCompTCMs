#'
#'@title Function to extract survey selectivity functions by year among several models
#'
#'@description This function extracts survey selectivity functions by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param fleets - names of survey fleets to include (or "all" or NULL to include all)
#'@param cast - formula to exclude factors from "averaging" over
#'@param years - vector of years to show, or 'all' to show all years
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Surveys.SelFcns<-function(objs,
                                      fleets="all",
                                      cast='y+x',
                                      years='all',
                                      verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Surveys.SelFcns().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.SurveyQuantities(obj,type="selSrv_yxz",verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Surveys.SelFcns(obj,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.SelFcns(obj,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (is.numeric(years)) mdfr <- mdfr[as.numeric(mdfr$y) %in% years,];

    if (verbose) cat("rCompTCMs::extractMDFR.Surveys.SelFcns: Done!\n");
    return(mdfr)
}
