#'
#'@title Function to plot cohort progression by year among several models
#'
#'@description This function plots cohort progression by year
#'   among several models.
#'
#'@param mdfr - melted data.frame with cohort progressions
#'@param facet_grid - formula for faceting using facet_grid
#'@param facet_wrap - formula for faceting using facet_wrap
#'@param dodge - width to dodge overlapping series
#'@param years - vector of years to include in plots (or NULL for all)
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing
#'@param lnscale - use log scale on y axis
#'@param scales - scales parameter for facet_grid/facet_wrap
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details If 'z' is a cast'ing factor, then a set of annual size composition plots are produced. Otherwise,
#'a set of time series plots are produced.
#'
#'@import ggplot2
#'
#'@export
#'
plotPop.CohortProgression<-function(mdfr,
                                   facet_grid=NULL,
                                   facet_wrap=NULL,
                                   dodge=0.2,
                                   years=NULL,
                                   mxy=4,
                                   nrow=2,
                                   lnscale=FALSE,
                                   scales="fixed",
                                   types=c("progression","byyear"),
                                   showPlot=FALSE,
                                   verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::plotPop.CohortProgression().\n");
    options(stringsAsFactors=FALSE);

    #----------------------------------
    #cohort progression by year
    #----------------------------------
    plots<-list();
    mdfr$z<-as.numeric(mdfr$z);
    mdfrp<-mdfr;
    if (!is.null(years)){
        idx<-as.numeric(mdfrp$y) %in% years;
        mdfrp<-mdfrp[idx,];
    }
    uY<-sort(unique(as.numeric(mdfrp$y)));
    uX<-sort(unique(mdfrp$x));

    if ("progression" %in% types){
        if (verbose) cat("Plotting progression\n");
        for (x in uX){
            idx<-mdfrp$x==x;
            mdfrpp<-mdfrp[idx,];
            mdfrpp$y<-factor(as.character(mdfrpp$y),levels=as.character(uY));
            ylim<-NULL;
            if (lnscale) ylim<-log(c(0.001*max(mdfrpp$val),NA));
            p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                           facet_grid=case+m+s~.,scales=scales,
                           xlab='size (mm CW)',ylab='Cohort Abundance',
                           units='millions',lnscale=lnscale,ylim=ylim,
                           title=paste0(x),
                           colour='y',guideTitleColour='year',
                           showPlot=FALSE);
            if (showPlot) print(p);
            cap<-paste0("\n  \nFigure &&figno. Cohort progression size comps for ",x,".  \n  \n");
            plots[[cap]]<-p;
        }#uX
    }

    if ("byyear" %in% types){
        if (verbose) cat("Plotting progression by year\n")
        uM<-sort(unique(mdfrp$m));
        uS<-sort(unique(mdfrp$s));
        for (x in uX){
            idx<-mdfrp$x==x;
                    for (pg in 1:ceiling(length(uY)/mxy)){
                        idy<-as.numeric(mdfrp$y) %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)];
                        mdfrpp<-mdfrp[idx&idy,];
                        if (nrow(mdfrpp)>0){
                            if (verbose) cat("Plotting ",x,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                            mdfrpp$y<-factor(as.character(mdfrpp$y),levels=as.character(uY));
                            p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                                           facet_wrap=~y,nrow=nrow,scales=scales,
                                           xlab='size (mm CW)',ylab='Cohort Abundance',
                                           units='millions',lnscale=lnscale,
                                           title=paste0(x),
                                           colour='case',guideTitleColor='',
                                           shape='m',guideTitleShape='',
                                           linetype='s',guideTitleLineType='',
                                           showPlot=FALSE);
                            if (showPlot) print(p);
                            cap<-paste0("\n  \nFigure &&figno. Cohort progression size comps for ",x,", (",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
                            plots[[cap]]<-p;
                        } else {
                            if (verbose) cat("Skipping ",x,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                        }
                    }#pg
        }#uX
    }
    if (verbose) cat("finished rCompTCMs::compareResults.Pop.CohortProgression().\n");
    return(plots)
}
