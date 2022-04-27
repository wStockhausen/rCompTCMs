#'
#'@title Extract time series of abundance or biommass data by fleet among several model scenarios
#'
#'@description Function to extract abundance or biomass time series data by fleet among
#'several model scenarios.
#'
#'@param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained', 'discard' or 'total')
#'@param data.type - data type ('abundance' or 'biomass')
#'@param fleets - vector of names of fleets to plot (or 'all')
#'@param sexs - vector of sexes to plot (or 'all')
#'@param maturity_states - vector of maturity states to plot (or 'all')
#'@param shell_conditions - vector of shell conditions to plot (or 'all')
#'@param ci - confidence intervals
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses [rTCSAM02::getMDFR.Data.FleetTimeSeries()].
#'
#'@return dataframe
#'
#'@importFrom rTCSAM02 getMDFR.Data.FleetTimeSeries
#'
#'@md
#'
#'@export
#'
extractMDFR.Data.FleetTimeSeriesABs<-function(objs=NULL,
                                              fleet.type=c('survey','fishery'),
                                              catch.type=c('index','retained','discard','total'),
                                              data.type=c('abundance','biomass'),
                                              fleets="all",
                                              sexs="all",
                                              maturity_states="all",
                                              shell_conditions="all",
                                              ci=0.80,
                                              verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::extractMDFR.Data.FleetTimeSeries().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) message("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))
            mdfr1<-rTCSAM02::getMDFR.Data.FleetTimeSeries(obj,
                                                          fleet.type=fleet.type,
                                                          data.type=data.type,
                                                          catch.type=catch.type,
                                                          ci=ci,
                                                          verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    if (fleets[1]          =="all") fleets          <-unique(mdfr$f);
    if (sexs[1]            =="all") sexs            <-unique(mdfr$x);
    if (maturity_states[1] =="all") maturity_states <-unique(mdfr$m);
    if (shell_conditions[1]=="all") shell_conditions<-unique(mdfr$s);
    mdfr<-mdfr[mdfr$f %in% fleets,];
    mdfr<-mdfr[mdfr$x %in% sexs,];
    mdfr<-mdfr[mdfr$m %in% maturity_states,];
    mdfr<-mdfr[mdfr$s %in% shell_conditions,];
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';

    if (verbose) message("Finished rCompTCMs::extractMDFR.Data.FleetTimeSeriesABs().\n");
    return(mdfr);
}
