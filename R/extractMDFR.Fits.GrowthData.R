#'
#'@title Extract fits to growth data among several model runs as a dataframe
#'
#'@description Function to extract fits to growth data among several model runs as a dataframe.
#'
#'@param objs - list of resLst objects
#'
#'@details None.
#'
#'@return dataframe
#'
#'@importFrom rTCSAM02 getMDFR.Fits.GrowthData
#'
#'@import dplyr
#'
#'@export
#'
extractMDFR.Fits.GrowthData<-function(objs){
    options(stringsAsFactors=FALSE);
    cases<-names(objs);

    lst = list();
    for (case in cases){
        mdfr1<-NULL;
        obj<-objs[[case]];
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.GrowthData(obj,FALSE);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            lst[[case]] = mdfr1;
        }
    }
    mdfr = dplyr::bind_rows(lst);

    if (is.null(mdfr)) {
        cat("\n \nNo fits to growth data.\n \n")
        return(NULL);
    }

    mdfr %<>% dplyr::mutate(z=as.numeric(z),
                            case=factor(case,levels=cases));
    return(mdfr);
}
