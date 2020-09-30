#'
#'@title Function to extract effective Ns from size comps by year among several models
#'
#'@description This function extracts effective Ns from size comps by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param fleets - names of fleets to include (or "all" or NULL to include all)
#'@param fleet.type - 'survey','fishery'
#'@param category - 'total','discard','retained','discard mortality', or 'index'
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe
#'
#'@details Results are extracted using \code{rTCSAM02::getMDFR.Fits.EffectiveNs}, as appropriate.
#'
#'@export
#'
extractMDFR.Fits.EffectiveNs<-function(objs,
                                       fleets="all",
                                       fleet.type=c("survey","fishery"),
                                       category=c('index','captured','discarded','retained','discard mortality'),
                                       verbose=FALSE){
    if (verbose) cat("--starting rCompTCMs::extractMDFR.Fits.EffectiveNs()\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    if (fleet.type[1]=="survey") category<-"index";

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.EffectiveNs(obj,
                                                                                        fleet.type=fleet.type[1],
                                                                                        catch.type=category[1],
                                                                                        verbose=verbose);
        if (!is.null(mdfr1)){
            if (verbose){
                cat("rCompTCMs::extractMDFR.Fits.EffectiveNs: names = ",paste(names(mdfr1),collapse=" "),"\n");
                cat("rCompTCMs::extractMDFR.Fits.EffectiveNs: fleets = ",paste(fleets,collapse=" "),"\n");
            }
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") {
                cat("rCompTCMs::extractMDFR.Fits.EffectiveNs: Extracting fleets\n")
                mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            }
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    if (verbose) cat("rCompTCMs::extractMDFR.Fits.EffectiveNs: Done!\n");
    return(mdfr);
}
