#'
#'@title Function to extract estimated survey abundance by year among several models as a dataframe
#'
#'@description This function extracts estimated survey abundance by year
#'   among several models as a dataframe.
#'   
#'@param objs - list of resLst objects
#'@param cast - casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
#'@param years - 'all' or vector of years to include
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return a dataframe in canonical format
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Surveys.Abundance<-function(objs,
                                        category='index',
                                        cast="x",
                                        years='all',
                                        verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Surveys.Abundance().\n");
    options(stringsAsFactors=FALSE);
    
    if (is.null(cast)){
        cat("Error in rCompTCMs::extractMDFR.Surveys.Abundance()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }
    
    category<-category[1];
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Surveys.Abundance(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Surveys.Abundance(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.Abundance(obj,category=category,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    
    if (is.numeric(years)) mdfr<-mdfr[mdfr$y %in% years,];

    if (verbose) cat("finished rCompTCMs::extractMDFR.Surveys.Abundance()!\n");
    return(mdfr)
}
