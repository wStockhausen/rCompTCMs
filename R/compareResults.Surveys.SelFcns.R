#'
#'@title Function to compare survey selectivity functions by year among several models
#'
#'@description This function compares survey selectivity functions by year
#'   among several models.
#'
#'@param objs - list of resLst objects or dataframe from call to \code{extractMDFR.Surveys.SelFcns}
#'@param cast - formula to exclude factors from "averaging" over
#'@param fleets - vector of survey fleets to plot, or "all"
#'@param years - vector of years to show, or 'all' to show all years
#'@param dodge - width to dodge overlapping series
#'@param singlePlot - flag to plot all years on single plot (be sure to adjust facet_grid)
#'@param mxy - max number of years per page
#'@param facet_wrap - ggplot2 formula to produce figure with wrapped facets
#'@param facet_grid - ggplot2 formula to produce figure with gridded facets
#'@param pdf - creates pdf, if not NULL
#'@param showPlot - flag (T/F) to show plot
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
compareResults.Surveys.SelFcns<-function(objs,
                                         cast='y+x',
                                         fleets="all",
                                         years='all',
                                         dodge=0.2,
                                         singlePlot=FALSE,
                                         mxy=15,
                                         facet_wrap=NULL,
                                         facet_grid=ifelse(singlePlot,"x~case","y~x"),
                                         pdf=NULL,
                                         showPlot=FALSE,
                                         verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Surveys.SelFcns().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(years)) return(list());

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (is.data.frame(objs)) {
        mdfr<-objs;
    } else {
        mdfr<-extractMDFR.Surveys.SelFcns(objs,fleets=fleets,cast=cast,years=years,verbose=verbose);
        if (is.null(mdfr)) return(list()); #empty list
    }

    #----------------------------------
    #selectivity functions
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    if (fleets[1]!="all") uF<-fleets;
    for (f in uF){
        if (verbose) cat("Plotting fleet",f,"\n")
        mdfrp<-mdfr[mdfr$fleet==f,];
        if (!singlePlot){
            uY<-unique(mdfrp$y);
            for (pg in 1:ceiling(length(uY)/mxy)){
                mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
                p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                               facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                               xlab='size (mm CW)',ylab='Selectivity',units='',lnscale=FALSE,
                               title=f,
                               colour='case',guideTitleColor='',
                               shape='case',guideTitleShape='',
                               showPlot=FALSE);
                if (showPlot||!is.null(pdf)) print(p);
                cap<-paste0("\n  \nFigure &&figno. Selectivity functions for ",f,"(",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
                plots[[cap]]<-p;
            }#pg
        } else {
            p<-plotMDFR.XY(mdfrp,x='z',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                           xlab='size (mm CW)',ylab='Selectivity',units='',lnscale=FALSE,
                           title=f,
                           colour='y',guideTitleColour='year',
                           shape='y',guideTitleShape='year',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            cap<-paste0("\n  \nFigure &&figno. Selectivity functions for ",f,".  \n  \n")
            plots[[cap]]<-p;
        }
    }#uF

    if (verbose) cat("rCompTCMs::compareResults.Surveys.SelFcns: Done!\n");
    return(plots)
}
