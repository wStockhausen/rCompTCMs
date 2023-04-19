#'
#'@title Function to extract cohort progression by year among several models as a dataframe
#'
#'@description This function extracts cohort progression by year
#'   among several models as a dataframe.
#'
#'@param objs - list of resLst objects
#'@param cast - casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
#'@param scaleToDensity - flag to scale abundance to 1-mm size bins
#'@param aggToCutpts - flag to aggregate (rebin) to provided cutpts
#'@param cutpts - cutpoints to aggregate to
#'@param years - 'all' or vector of years to include
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe in canonical format
#'
#'@details Results are extracted using \code{rTCSAM2013::getMDFR.Pop.Abundance},
#'\code{rsimTCSAM::getMDFR.Pop.Abundance}, and/or [rTCSAM02::getMDFR.Pop.Abundance()], as appropriate., and
#'cast to aggregate. This differs from \code{extractMDFR.Pop.Abundance1}.
#'
#' If scaleToDensity is true, the size distribution after cast'ing is scaled to abundance/mm to allow easier comparison between models
#'with different bin sizes. If aggToCutpts is true, the distribution(s) are re-binned (aggregated) to a common set of
#'cutpoints.
#'
#'@export
#'
extractMDFR.Pop.CohortProgression<-function(objs,
                                            cast="x+m+s+z",
                                            scaleToDensity=FALSE,
                                            aggToCutpts=FALSE,
                                            cutpts=seq(25,185,5),
                                            years='all',
                                            verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::extractMDFR.Pop.CohortProgression().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(cast)){
        cat("Error in rCompTCMs::extractMDFR.Pop.CohortProgression()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   {
            mdfr1<-rTCSAM02::getMDFR.Pop.CohortProgression(obj,cast=cast,verbose=verbose);
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
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);

    if (is.numeric(years)) {
        mdfr<-mdfr[mdfr$y %in% years,];
    }

    if (verbose) cat("finished rCompTCMs::extractMDFR.Pop.CohortProgression().\n");
    return(mdfr)
}
