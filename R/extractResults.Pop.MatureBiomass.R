#'
#'@title Function to extract mature biomass estimates by year among several models
#'
#'@description This function extracts mature biomass estimates by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return datafram in canonical format
#'
#'@details uses \code{rTCSAM2013::getMDFR.Pop.Quantities}, 
#'\code{rsimTCSAM::getMDFR.Pop.Quantities}, \code{rsimTCSAM::getMDFR.Pop.Quantities}.
#'
#'@export
#'
extractResults.Pop.MatureBiomass<-function(objs,
                                           verbose=TRUE){
    if (verbose) cat("Starting rCompTCMs::extractResults.Pop.MatureBiomass().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.Quantities(obj,type="MB_yx",verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type="MB_yx",verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type="MB_yx",verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    return(mdfr);
}
