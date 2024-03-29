#'
#'@title Function to compare estimated survey biomass by year among several models
#'
#'@description This function extracts estimated survey biomass by year
#'   among several models as a dataframe
#'
#'@param objs - list of resLst objects
#'@param fleets - names of fleets to include (or "all")
#'@param cast - cast'ing formula for aggregating by factors (y,x,m,s,z)
#'@param years - 'all' or vector of years to include
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe in canonical format.
#'
#'@import magrittr
#'@import dplyr
#'@import stringr
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Surveys.Biomass<-function(objs,
                                      fleets="all",
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

    fleets %<>% stringr::str_replace(stringr::fixed("_"),stringr::fixed(" "));

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.Biomass(obj,category=category,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1 %<>% dplyr::filter(fleet %in% fleets);
            mdfr1$case<-case;
            mdfr<-dplyr::bind_rows(mdfr,mdfr1);
        }
    }
    mdfr %<>% dplyr::mutate(case=factor(mdfr$case,levels=cases),
                            y=as.numeric(y));

    if (is.numeric(years)) mdfr %<>% dplyr::filter(y %in% years);

    if (verbose) cat("finished rCompTCMs::extractMDFR.Surveys.Biomass(: Done))!\n");
    return(mdfr)
}
