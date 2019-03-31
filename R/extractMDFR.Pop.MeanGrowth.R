#'
#'@title Extract mean post-molt size from several model runs
#'
#'@description Function to extract mean post-molt size from several model runs.
#'
#'@param objs - list of resLst objects
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details None.
#'
#'@return dataframe in caonical format
#'
#'@export
#'
extractMDFR.Pop.MeanGrowth<-function(objs,
                                     verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.MeanGrowth(obj,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.MeanGrowth(obj,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.MeanGrowth(obj,verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);

    return(mdfr);
}
