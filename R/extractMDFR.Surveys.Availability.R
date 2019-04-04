#'
#'@title Function to extract survey availabilities
#'
#'@description This function extracts survey availability estimates by year,
#'   sex and maturity state.
#'
#'@param objs - list of resLst objects
#'@param years - vector of years to show, or 'all' to show all years
#'@param cast - formula to exclude factors from "averaging" over (x,m,s)
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe in canonical format
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Surveys.Availability<-function(objs,
                                           years='all',
                                           cast='x',
                                           verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Surveys.Availability().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL; #rsimTCSAM::getMDFR.SurveysAvailability(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.Availability(obj,cast=cast,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    if (is.null(mdfr)) return(NULL);

    mdfr$y<-as.numeric(mdfr$y)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (is.numeric(years)) mdfr <- mdfr[as.numeric(mdfr$y) %in% years,];

    if (verbose) cat("Finished rCompTCMs::extractMDFR.Surveys.Availability().\n");
    return(mdfr)
}
