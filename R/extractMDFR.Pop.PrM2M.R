#'
#'@title Extract probability of molt-to-maturity from several model runs
#'
#'@description Function to extract probability of molt-to-maturity from several model runs.
#'
#'@param obj - list of resLst objects
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details None.
#'
#'@return dataframe in canonical format
#'
#'@export
#'
extractMDFR.Pop.PrM2M<-function(objs,
                                verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.PrM2M(obj,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.PrM2M(obj,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.PrM2M(obj,verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);

    return(mdfr);
}
