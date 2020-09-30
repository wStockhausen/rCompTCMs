#'
#'@title Extract Pearson's or nll residuals from size comps by fleet among several model runs as dataframe
#'
#'@description Function to extract Pearson's or nll residuals from size comps by fleet among
#'several model runs.
#'
#' @param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#' @param fleets - names of fleets to include (or "all" or NULL to include all)
#' @param tcsam2013.type - pearsons residuals type for tcsam2013 models ("PRs_yxmz","PRs_yxz")
#' @param fleet.type - fleet type ('fishery' or 'survey')
#' @param catch.type - catch type ('index','retained',or 'total')
#' @param residuals.type - residual type for tcsam02 models ('pearsons' or 'nlls')
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, \code{rTCSAM02::getMDFR.ZScores.PrNatZ()}.
#'Also uses \code{wtsUtilities::printGGList}.
#'
#'@return dataframe in format for \code{compareFits.ZScores.PrNatZ}.
#'
#'@export
#'
extractFits.ZScores.PrNatZ<-function(objs=NULL,
                                     fleets="all",
                                     fleet.type=c('survey','fishery'),
                                     catch.type=c('index','retained','total'),
                                     residuals.type=c('pearsons','nlls'),
                                     tcsam2013.type=c("PRs_yxmz","PRs_yxz"),
                                     verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::extractFits.ZScores.PrNatZ().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    residuals.type<-residuals.type[1];
    tcsam2013.type<-tcsam2013.type[1];

    if (fleet.type=='survey') catch.type<-'index';
    if ((fleet.type=='fishery')&&(catch.type=='retained')) tcsam2013.type<-'PRs.ret';
    if ((fleet.type=='fishery')&&(catch.type=='total'))    tcsam2013.type<-'PRs.tot';

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (fleet.type=='survey'){
            if (inherits(obj,"tcsam2013.resLst"))
                mdfr1<-rTCSAM2013::getMDFR.SurveyQuantities(obj,
                                                            type=tcsam2013.type,
                                                            verbose=verbose);
            if (inherits(obj,"tcsam02.resLst"))
                mdfr1<-rTCSAM02::getMDFR.ZScores.PrNatZ(obj,
                                                        fleet.type=fleet.type,
                                                        catch.type='index',
                                                        residuals.type=residuals.type,
                                                        verbose=verbose);
        }
        if (fleet.type=='fishery'){
            if (inherits(obj,"tcsam2013.resLst"))
                mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,
                                                             type=tcsam2013.type,
                                                             verbose=verbose);
            if (inherits(obj,"tcsam02.resLst"))
                mdfr1<-rTCSAM02::getMDFR.ZScores.PrNatZ(obj,
                                                        fleet.type=fleet.type,
                                                        catch.type=catch.type,
                                                        residuals.type=residuals.type,
                                                        verbose=verbose);
        }
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$z<-as.numeric(mdfr$z);

    mdfr$sign<-ifelse(test=mdfr$val>0,yes=">0",no="<0");
    mdfr$val <- abs(mdfr$val);

    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';


    if (verbose) cat("Finished rCompTCMs::extractFits.ZScores.PrNatZ().\n");
    return(mdfr);
}
