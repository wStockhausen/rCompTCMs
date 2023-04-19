#'
#'@title Extract recruitment size distributions from several model runs
#'
#'@description Function to extract recruitment size distributions from several model runs.
#'
#'@param objs - list of resLst objects
#'@param scaleToDensity - flag to scale abundance to 1-mm size bins
#'@param aggToCutpts - flag to aggregate (rebin) to provided cutpts
#'@param cutpts - cutpoints to aggregate to
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details If scaleToDensity is true, the size distribution is scaled to abundance/mm to allow easier comparison between models
#'with different bin sizes. If aggToCutpts is true, the distribution(s) are re-binned (aggregated) to a common set of
#'cutpoints.
#'
#'@return dataframe in canonical format
#'
#'@importFrom dplyr mutate
#'@importFrom rTCSAM02 getMDFR.Pop.RecSizeDistribution
#'
#'@export
#'
extractMDFR.Pop.RecSizeDistribution<-function(objs,
                                              scaleToDensity=FALSE,
                                              aggToCutpts=FALSE,
                                              cutpts=seq(25,185,5),
                                              verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::extractMDFR.Pop.RecSizeDistribution().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) {
            mdfr1<-rTCSAM2013::getMDFR.Pop.RecSizeDistribution(obj,verbose);
        }
        if (inherits(obj,"rsimTCSAM.resLst")) {
            mdfr1<-rsimTCSAM::getMDFR.Pop.RecSizeDistribution(obj,verbose);
        }
        if (inherits(obj,"tcsam02.resLst")) {
            mdfr1 = rTCSAM02::getMDFR.Pop.RecSizeDistribution(obj,verbose);
            if (scaleToDensity){
                zCs = obj$rep$mc$dims$zc$vls;
                dZ  = zCs[2]-zCs[1];
                mdfr1 = mdfr1 |>
                         dplyr::mutate(val=val/dZ,
                                       lci=lci/dZ,
                                       uci=uci/dZ);
            }
            if (aggToCutpts) {
                dz = cutpts[2]-cutpts[1];
                mdfr1$z = wtsUtilities::applyCutPts(as.numeric(mdfr1$z),seq(25,185,5),FALSE,FALSE) + dz/2;
                vars = names(mdfr1)[names(mdfr1) %in% c("val","lci","uci")];
                facs = names(mdfr1)[!(names(mdfr1) %in% vars)];
                mdfr1 = wtsUtilities::sumBy(mdfr1,facs,vars);
            }
        }
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-as.numeric(mdfr$z);
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (verbose) cat("--finished rCompTCMs::extractMDFR.Pop.RecSizeDistribution().\n");
    return(mdfr);
}

