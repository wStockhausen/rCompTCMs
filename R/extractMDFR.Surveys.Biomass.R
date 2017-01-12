#'
#'@title Function to compare estimated survey biomass by year among several models
#'
#'@description This function extracts estimated survey biomass by year
#'   among several models as a dataframe
#'   
#'@param objs - list of resLst objects
#'@param cast - cast'ing formula for aggregating by factors (y,x,m,s,z)
#'@param years - 'all' or vector of years to include
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe in canonical format.
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Surveys.Biomass<-function(objs,
                                         category='index',
                                         cast="y+x",
                                         years='all',
                                         verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Surveys.Biomass().\n");
    options(stringsAsFactors=FALSE);
    
    if (is.null(cast)){
        cat("Error in rCompTCMs::extractMDFR.Surveys.Biomass()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }
    
    category<-"index";
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    
    if (is.numeric(years)) mdfr<-mdfr[mdfr$y %in% years,];
    
    if (verbose) cat("finished rCompTCMs::extractMDFR.Surveys.Biomass(: Done))!\n");
    return(mdfr)
}
