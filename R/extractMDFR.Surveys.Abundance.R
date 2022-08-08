#'
#'@title Function to extract estimated survey abundance by year among several models as a dataframe
#'
#'@description This function extracts estimated survey abundance by year
#'   among several models as a dataframe.
#'
#'@param objs - list of resLst objects
#'@param fleets - names of fleets to include (or "all")
#'@param cast - casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
#'@param years - 'all' or vector of years to include
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return a dataframe in canonical format
#'
#'@details None.
#'
#'@import magrittr
#'@import dplyr
#'@import stringr
#'
#'@export
#'
extractMDFR.Surveys.Abundance<-function(objs,
                                        fleets="all",
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

    category<-"index";

    cases<-names(objs);

    fleets %<>% stringr::str_replace(stringr::fixed("_"),stringr::fixed(" "));

    mdfr<-NULL;
    for (case in cases){
        #--testing: case = cases[1];
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Surveys.Abundance(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Surveys.Abundance(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.Abundance(obj,category=category,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1 %<>% dplyr::filter(fleet %in% fleets);
            mdfr1$case<-case;
            mdfr<-dplyr::bind_rows(mdfr,mdfr1);
        }
    }
    mdfr %<>% dplyr::mutate(case=factor(mdfr$case,levels=cases),
                            y=as.numeric(y));

    if (is.numeric(years)) mdfr %<>% dplyr::filter(y %in% years);

    if (verbose) cat("finished rCompTCMs::extractMDFR.Surveys.Abundance()!\n");
    return(mdfr)
}
