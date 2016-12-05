#'
#'@title Function to extract survey catchabilities
#'
#'@description This function extracts survey catchability estimates by year,
#'   sex and maturity state.
#'   
#'@param objs - list of resLst objects
#'@param cast - formula to exclude factors from "averaging" over (x,m,s)
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe in canonical format
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Surveys.Catchability<-function(objs,
                                           cast='x',
                                           verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Surveys.Catchability().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.SurveyQuantities(obj,type="qSrv_xy",verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL; #rsimTCSAM::getMDFR.SurveysCatchability(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.Catchability(obj,cast=cast,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (verbose) cat("Finished rCompTCMs::extractMDFR.Surveys.Catchability().\n");
    return(mdfr)
}
