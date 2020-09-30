#'
#'@title Function to extract survey capture probability functions by year among several models
#'
#'@description This function extracts survey capture probability functions by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param fleets - names of fleets to include (or "all")
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
extractMDFR.Surveys.CaptureProbs<-function(objs,
                                           fleets="all",
                                           cast='y+x',
                                           years='all',
                                           verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Surveys.CaptureProbs().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.CaptureProbs(obj,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    if (is.null(mdfr)) return(NULL);

    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (is.numeric(years)) mdfr <- mdfr[as.numeric(mdfr$y) %in% years,];

    if (verbose) cat("rCompTCMs::extractMDFR.Surveys.CaptureProbs: Done!\n");
    return(mdfr)
}
