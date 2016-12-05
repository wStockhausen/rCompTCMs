#'
#'@title Function to extract fishery catchabilities by year using ggplot2
#'
#'@description This function extracts fishery catchability estimates by year,
#'   sex and maturity state.
#'   
#'@param objs - list of resLst objects
#'@param cast - formula to exclude factors from "averaging" over
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe in canonical format
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Fisheries.Catchability<-function(objs,
                                             cast='x',
                                             verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Fisheries.Catchability().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,type="qFsh_xy",verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL; #rsimTCSAM::getMDFR.Fisheries.Catchability(obj,cast=cast,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fisheries.Catchability(obj,cast=cast,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y)
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    if (verbose) cat("Finished rCompTCMs::extractMDFR.Fisheries.Catchability().\n");
    return(mdfr)
}
