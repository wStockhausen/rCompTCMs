#'
#'@title Function to extract estimated fishery catch biomass by year among several models as a dataframe
#'
#'@description This function extracts estimated fishery catch biomass by year
#'   among several models as a dataframe.
#'
#'@param objs - list of resLst objects
#'@param category - 'captured','discarded','retained','discard mortality', or 'index'
#'@param cast - cast'ing formula for aggregating by factors (y,x,m,s,z)
#'@param years - 'all' or vector of years to include
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Fisheries.CatchBiomass<-function(objs,
                                             category=c('captured','discarded','retained','discard mortality','index'),
                                             cast=NULL,
                                             years='all',
                                             verbose=FALSE){
    if (verbose) cat("--starting rCompTCMs::extractMDFR.Fisheries.CatchBiomass().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(cast)){
        cat("Error in rCompTCMs::extractMDFR.Fisheries.CatchBiomass()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }

    category<-category[1];

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Fisheries.CatchBiomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Fisheries.CatchBiomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fisheries.CatchBiomass(obj,category=category,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);

    if (is.numeric(years)) {
        mdfr<-mdfr[mdfr$y %in% years,];
    }

    if (verbose) cat("--finished rCompTCMs::extractMDFR.Fisheries.CatchBiomass().\n");
    return(mdfr)
}
