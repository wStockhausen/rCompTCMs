#'
#'@title Function to plot natural mortality rates by year using ggplot2
#'
#'@description This function plots natural mortality estimates by year,
#'   sex and maturity state.
#'
#'@param objs - list of resLst objects
#'@param type - type of estimates to return
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
extractMDFR.Pop.NaturalMortality<-function(objs,
                                           type="M_yxm",
                                           verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.NaturalMortality(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.NaturalMortality(obj,type,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-getMDFR.Pop.NaturalMortality(obj,type,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y)
    mdfr$case<-factor(mdfr$case,levels=cases);

    return(mdfr);
}

