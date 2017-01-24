#'
#'@title Extract recruitment size distributions from several model runs
#'
#'@description Function to extract recruitment size distributions from several model runs.
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
extractMDFR.Pop.RecSizeDistribution<-function(objs,
                                              verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::extractMDFR.Pop.RecSizeDistribution().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.RecSizeDistribution(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.RecSizeDistribution(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.RecSizeDistribution(obj,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    if (verbose) cat("--finished rCompTCMs::extractMDFR.Pop.RecSizeDistribution().\n");
    return(mdfr);
}

