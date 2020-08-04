#'
#'@title Extract fits to biomass time series by fleet among several model runs
#'
#'@description Function to extract fits to biomass time series by fleet among
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained',  or 'total')
#'@param ci - confidence interval for plots
#'@param fishery.pdfType - assumed distribution for fishery data
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, \code{rTCSAM02::getMDFR.Fits.FleetData()}.
#'
#'@return dataframe
#'
#'@export
#'
extractMDFR.Fits.BiomassData<-function(objs=NULL,
                                       fleet.type=c('survey','fishery'),
                                       catch.type=c('index','retained','discard','total'),
                                       ci=0.80,
                                       fishery.pdfType=c("norm2","normal","lognormal"),
                                       verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::extractMDFR.Fits.BiomassData().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (verbose) cat("fleet.type=",fleet.type,"\n","catch.type=",catch.type,"\n",sep="");

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);
    if (verbose) cat("cases:",cases,"\n");

    if (catch.type=='retained') type<-'bio.retm';
    if (catch.type=='discard')  type<-'bio.dscm';
    if (catch.type=='total')    type<-'bio.totm';

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.FleetData(obj,
                                                                                      fleet.type=fleet.type,
                                                                                      data.type='biomass',
                                                                                      catch.type=catch.type,
                                                                                      ci=ci,
                                                                                      verbose=verbose);
        if (fleet.type=='survey'){
            if (inherits(obj,"tcsam2013.resLst"))
            mdfr1<-rTCSAM2013::getMDFR.SurveyQuantities(obj,
                                                        type='MB_yx',
                                                        pdfType='lognormal',
                                                        ci=ci,
                                                        verbose=verbose);
        }
        if (fleet.type=='fishery'){
            if (inherits(obj,"tcsam2013.resLst"))
                mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,
                                                             type=type,
                                                             pdfType=fishery.pdfType,
                                                             ci=ci,
                                                             verbose=verbose);
        }
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';

    print(str(mdfr));

    if (verbose) if(is.null(mdfr)) cat("mdfr is NULL\n");
    if (verbose) cat("nrow(mdfr) =",nrow(mdfr),"\n");
    if (verbose) cat("Finished rCompTCMs::extractMDFR.Fits.BiomassData().\n");
    return(mdfr);
}
