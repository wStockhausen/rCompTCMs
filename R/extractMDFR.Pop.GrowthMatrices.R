#'
#'@title Extract growth matrices among several model runs
#'
#'@description Function to extract growth matrices among several model runs.
#'
#'@param objs - list of resLst objects
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details None.
#'
#'@return dataframe in canonical format
#'
#'@export
#'
extractMDFR.Pop.GrowthMatrices<-function(objs,
                                         verbose=TRUE){

    if (verbose) cat("Starting rCompTCMs::extractMDFR.Pop.GrowthMatrices().\n")
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);
    
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.GrowthMatrices(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.GrowthMatrices(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.GrowthMatrices(obj,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-as.numeric(mdfr$z);
    mdfr$zp<-as.numeric(mdfr$zp);
    mdfr$case<-factor(mdfr$case,levels=cases);
    

    if (verbose) cat("Finished rCompTCMs::extractMDFR.Pop.GrowthMatrices().\n")
    return(mdfr);
}
