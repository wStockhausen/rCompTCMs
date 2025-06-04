#'
#' @title Extract sd_report info on recruitment and SSB time series estimates as a dataframe
#'
#' @description Function to extract sd_report info on recruitment and SSB time series estimates as a dataframe.
#'
#' @param objs - list of resLst objects
#' @param ci - confidence interval
#' @param verbose - flag to print debugging info
#'
#' @return a dataframe
#'
#' @details Delegates to [rTCSAM02::getMDFR.SdRep.RecAndSSB()]. Recruitment values are on ln-scale.
#'
#' @importFrom rTCSAM02 getMDFR.SdRep.RecAndSSB
#'
#' @export
#'
extractMDFR.SdRep.RecAndSSB<-function(objs,
                                      ci=0.95,
                                      verbose=FALSE){
    options(stringsAsFactors=FALSE);
    cases = names(objs);
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        mdfr1 = rTCSAM02::getMDFR.SdRep.RecAndSSB(obj)
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$y<-as.numeric(mdfr$y)
    mdfr$case<-factor(mdfr$case,levels=cases);
    return(mdfr);
}
