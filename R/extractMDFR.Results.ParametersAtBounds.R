#'
#'@title Extract parameters at one of their bounds from several model runs as a dataframe
#'
#'@description This function extracts parameters at one of their bounds from several model runs as a dataframe.
#'   
#'@param objs - list of resLst objects
#'@param delta - relative fraction of range defining "at bounds"
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe
#'
#'@details Results are extracted using \code{rTCSAM02::getMDFR.ParametersAtBounds} for tcsam02 model runs.
#'
#'@export
#'
extractMDFR.Results.ParametersAtBounds<-function(objs,
                                                 delta=0.0001,
                                                 verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::extractMDFR.Results.ParametersAtBounds().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.ParametersAtBounds(obj,delta=delta,verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (verbose) cat("finished rCompTCMs::extractMDFR.Results.ParametersAtBounds().\n");
    return(mdfr)
}
