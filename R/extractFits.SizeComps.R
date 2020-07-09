#'
#'@title Extract fits to size comps by fleet from several model runs
#'
#'@description Function to extract fits to size comps by fleet from
#'several model runs.
#'
#' @param obj - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#' @param fleet.type - fleet type ('fishery' or 'survey')
#' @param catch.type - catch type ('index','retained',  or 'total')
#' @param  years - years to plot, as numerical vector (or "all" to plot all years)
#' @param plot1stObs - flag (T/F) to plot observations only from first case
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, \code{rTCSAM02::getMDFR.Fits.FleetData()}.
#'
#'@return dataframe in canonical format
#'
#'@export
#'
extractFits.SizeComps<-function(objs=NULL,
                                 fleets="all",
                                  fleet.type=c('survey','fishery'),
                                  catch.type=c('index','retained','discard','total'),
                                  years='all',
                                  plot1stObs=TRUE,
                                  verbose=FALSE){

    if (verbose) {
        cat("Starting rCompTCMs::extractFits.SizeComps().\n");
        cat("Extracting fleet.type = ",fleet.type,", catch.type = ",catch.type,"for",fleets,"\n");
    }
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);

    if (catch.type=='index')    type<-'prNatZ_yxmz';
    if (catch.type=='retained') type<-'prNatZ.ret';
    if (catch.type=='total')    type<-'prNatZ.tot';

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.FleetData(obj,
                                                                                      fleet.type=fleet.type,
                                                                                      data.type='n.at.z',
                                                                                      catch.type=catch.type,
                                                                                      verbose=verbose);
        if (inherits(obj,"tcsam2013.resLst")){
            if (fleet.type=='survey'){
                mdfr1<-rTCSAM2013::getMDFR.SurveyQuantities(obj,
                                                            type=type,
                                                            verbose=verbose);
            }
            if (fleet.type=='fishery'){
                mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,
                                                             type=type,
                                                             verbose=verbose);
            }
            #adjust to bin center
            mdfr1$z<-mdfr1$z+0.5;
        }
        if (!is.null(mdfr1)){
            if (fleets!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            if (nrow(mdfr1)>0){
                mdfr1$case<-case;
                mdfr<-rbind(mdfr,mdfr1);
            }
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';

    if (is.numeric(years)) mdfr<-mdfr[mdfr$y %in% years,];

    if (plot1stObs){
        #drop observations from all cases except the first available by fleet
        # idx<-(as.character(mdfr$case)==cases[1])&(mdfr$type=="observed")|(mdfr$type=="predicted");
        # mdfr<-mdfr[idx,];
        #keep all predicted
        mdfrp<-mdfr[mdfr$type=="predicted",];
        #by fleet, get first case with observations
        mdfro<-mdfr[mdfr$type=="observed",];
        fleets<-unique(mdfr$fleet);
        for (fleet in fleets) {
            if (verbose) cat("Checking",fleet,"for model case with first observations.\n")
            mdfrof<-mdfro[(mdfro$fleet==fleet),];
            uCs<-unique(mdfrof$case);
            if (verbose) cat("--These cases were found to include observations for this fleet:",paste(uCs,collapse=", "),"\n");
            if (length(uCs)>0) {
                idc<-mdfrof$case==uCs[1];
                if (verbose) cat("--Using model case",uCs[1],"for first observations; found",sum(idc,na.rm=TRUE),"\n");
                mdfrp<-rbind(mdfrp,mdfrof[idc,]);
            }
        }
        mdfr<-mdfrp;
    }

    if (verbose) cat("Finished rCompTCMs::extractFits.SizeComps().\n");
    return(mdfr);
}
