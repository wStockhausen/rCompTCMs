#'
#' @title Extract fits to maturity ogive data among several model runs as a dataframe
#'
#' @description Function to extract fits to maturity ogive data among several model runs as a dataframe.
#'
#' @param objs - list of tcsam02 resLst objects
#'
#' @details None.
#'
#' @return dataframe
#'
#' @importFrom rTCSAM02 getMDFR.Fits.MaturityOgiveData
#'
#' @export
#'
extractMDFR.Fits.MaturityOgiveData<-function(objs){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        mdfr1<-NULL;
        obj<-objs[[case]];
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.MaturityOgiveData(obj,FALSE);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }

    if (is.null(mdfr)) {
        cat("\n \nNo fits to maturity data.\n \n")
        return(NULL);
    }

    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    return(mdfr);
}
