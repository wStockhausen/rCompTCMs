#'
#'@title Function to compare fishery selectivity functions by year among several models
#'
#'@description This function compares fishery selectivity functions by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param fleets - names of fleets to include (or "all")
#'@param cast - formula to exclude factors from "averaging" over
#'@param years - vector of years to show, or 'all' to show all years
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
extractMDFR.Fisheries.SelFcns<-function(objs,
                                        fleets="all",
                                        cast='y+x',
                                        years='all',
                                        verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Fisheries.SelFcns().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,type='sel_yxz',verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Fisheries.SelFcns(obj,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fisheries.SelFcns(obj,cast=cast,verbose=verbose);
        if (nrow(mdfr1)>0){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1 %<>% dplyr::filter(fleet %in% fleets);
            if (nrow(mdfr1)>0){
                mdfr1$case<-case;
                mdfr<-dplyr::bind_rows(mdfr,mdfr1);
            } else {
                if (verbose) warning("case '",case,"' had zero rows.")
            }
        }
    }
    if (is.null(mdfr)) {
        warning("rCompTCMs::extractMDFR.Fisheries.SelFcns:: Returning NULL object!")
        return(mdfr);
    }

    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (is.numeric(years)) mdfr %<>% dplyr::filter(as.numeric(y) %in% years);

    if (verbose) cat("rCompTCMs::extractMDFR.Fisheries.SelFcns: Done!\n");
    return(mdfr)
}
