#'
#'@title Function to compare fishery retention functions by year among several models
#'
#'@description This function compares fishery retention functions by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param cast - formula to exclude factors from "averaging" over
#'@param fleets - vector of feets to plot, or "all"
#'@param years - vector of years to show, or 'all' to show all years
#'@param dodge - width to dodge overlapping series
#'@param mxy - max number of years per page
#'@param singlePlot - flag to plot all years on single plot (be sure to adjust facet_grid)
#'@param facet_wrap - ggplot2 formula to produce figure with wrapped facets
#'@param facet_grid - ggplot2 formula to produce figure with gridded facets
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return lists ofggplot2 objects, nested by fishery or an empty list if year is NULL
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Fisheries.RetFcns<-function(objs,
                                           cast='y+x',
                                           fleets="all",
                                           years='all',
                                           dodge=0.2,
                                           mxy=15,
                                           singlePlot=FALSE,
                                           facet_wrap=NULL,
                                           facet_grid=ifelse(singlePlot,"x~case","y~x"),
                                           showPlot=FALSE,
                                           pdf=NULL,
                                           verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Fisheries.RetFcns().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(years)) return(list());

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,type='ret_yxz',verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Fisheries.RetFcns(obj,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fisheries.RetFcns(obj,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (is.numeric(years)) mdfr <- mdfr[as.numeric(mdfr$y) %in% years,];

    #----------------------------------
    #retention functions
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    if (fleets[1]!="all") uF<-fleets;
    for (f in uF){
        if (verbose) cat("Plotting fleet",f,"\n")
        mdfrp<-mdfr[mdfr$fleet==f,];
        uY<-unique(mdfrp$y);
        subPlots<-list();
        if(!singlePlot){
            for (pg in 1:ceiling(length(uY)/mxy)){
                mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
                p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                               facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                               xlab='size (mm CW)',ylab='retention',units='',lnscale=FALSE,
                               title=f,
                               colour='case',guideTitleColor='',
                               shape='case',guideTitleShape='',
                               showPlot=FALSE);
                if (showPlot||!is.null(pdf)) print(p);
                cap<-paste0("\n  \nFigure &&figno. Retention functions for ",f,"(",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
                subPlots[[cap]]<-p;
            }#pg
        } else {
            p<-plotMDFR.XY(mdfrp,x='z',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                           xlab='size (mm CW)',ylab='retention',units='',lnscale=FALSE,
                           title=f,
                           colour='y',guideTitleColour='year',
                           shape='y',guideTitleShape='year',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            cap<-paste0("\n  \nFigure &&figno. Retention functions for ",f,".  \n  \n")
            subPlots[[cap]]<-p;
        }
        plots[[f]]<-subPlots;
    }#uF

    if (verbose) cat("rCompTCMs::compareResults.Fisheries.RetFcns(): Done!\n");
    return(plots)
}
