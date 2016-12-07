#'
#'@title Extract fits to mean size comps by fleet among several model runs
#'
#'@description Function to extract fits to mean size comps by fleet among 
#'several model runs.
#'
#' @param obj - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#' @param fleet.type - fleet type ('fishery' or 'survey')
#' @param catch.type - catch type ('index','retained',  or 'total')
#' @param  years - years to plot, as numerical vector (or "all" to plot all years)
#' @param plot1stObs - flag (T/F) to plot observations only from first case
#' @param ci - confidence interval
#' @param facet_grid - faceting formula for ggplot2::facet_grid()
#' @param pdf - name for output pdf file
#' @param showPlot - flag (T/F) to show plot
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, \code{rTCSAM02::getMDFR.Fits.FleetData()}.
#'Also uses \code{reshape2::dcast}, \code{wtsUtilities::calcCIs}.
#'
#'@return dataframe in canonical format
#'
#'@export
#'
extractFits.MeanSizeComps<-function(objs=NULL,
                                    fleet.type=c('survey','fishery'),
                                    catch.type=c('index','retained','discard','total'),
                                    years='all',
                                    ci=0.80,
                                    plot1stObs=TRUE,
                                    verbose=FALSE){
    
    if (verbose) cat("Starting rCompTCMs::extractFits.MeanSizeComps().\n");
    options(stringsAsFactors=FALSE);
    
    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);

    if (catch.type=='index')    type<-'prNatZ_yxmz';
    if (catch.type=='retained') type<-'prNatZ.ret';
    if (catch.type=='total')    type<-'prNatZ.tot';
    
    #get annual size comps
    mdfr<-rCompTCMs::extractFits.SizeComps(objs,
                                           fleet.type=fleet.type,
                                           catch.type=catch.type,
                                           years=years,
                                           plot1stObs=plot1stObs,
                                           verbose=verbose);

    #compute averages over years
    dfr1<-reshape2::dcast(mdfr,formula="case+process+fleet+category+type+x+m+s+z~.",fun.aggregate=mean,  na.rm=TRUE,value.var="val")
    dfr2<-reshape2::dcast(mdfr,formula="case+process+fleet+category+type+x+m+s+z~.",fun.aggregate=sd,    na.rm=TRUE,value.var="val");
    dfr3<-reshape2::dcast(mdfr,formula="case+process+fleet+category+type+x+m+s+z~.",fun.aggregate=length,value.var="val");
    names(dfr1)[10]<-'val';
    names(dfr2)[10]<-'sd';
    mdfr<-dfr1;
    res<-wtsUtilities::calcCIs(mdfr$val,sdvs=dfr2$sd,pdfType="normal",ci=ci);
    mdfr$lci<-res$lci;
    mdfr$uci<-res$uci;
    mdfr$N  <-dfr3[["."]];

    #mdfr<-getMDFR.CanonicalFormat(mdfr);
    return(mdfr);
}
