#'
#'@title Function to compare estimated fishery catch abundance by year among several models
#'
#'@description This function compares estimated fishery catch abundance by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param fleets - names of fleets to include (or "all")
#'@param category - 'captured','discarded','retained','discard mortality', or 'index'
#'@param cast - cast'ing formula for aggregating by factors (x,m,s,z)
#'@param facet_grid - formula (or string version of formula) for faceting using facet_grid
#'@param facet_wrap - one-sided formula (e.g., "~y+x") or character vector (e.g., c('y','x')) for faceting using facet_wrap
#'@param scales - scales parameter for use with facet_grid/facet_wrap
#'@param dodge - width to dodge overlapping series
#'@param years - 'all' or vector of years to include
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing
#'@param lnscale - use log scale on y axis
#'@param plotPoints - flag to plot points
#'@param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#'@param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return list of ggplot2 objects
#'
#'@details If 'z' is a cast'ing factor, then a set of annual size composition plots are produced. Otherwise,
#'a set of time series plots are produced.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Fisheries.CatchAbundance<-function(objs,
                                                  fleets="all",
                                           category=c('captured','discarded','retained','discard mortality','index'),
                                           cast=NULL,
                                           facet_grid=NULL,
                                           facet_wrap=NULL,
                                           scales="fixed",
                                           dodge=0.2,
                                           years='all',
                                           mxy=15,
                                           nrow=5,
                                           lnscale=FALSE,
                                           plotPoints=TRUE,
                                           colour_scale=NULL,
                                           fill_scale=NULL,
                                           showPlot=FALSE,
                                           pdf=NULL,
                                           verbose=FALSE){
    if (verbose) cat("--starting rCompTCMs::compareResults.Fisheries.CatchAbundance().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(cast)){
        cat("Error in rCompTCMs::compareResults.Fisheries.CatchAbundance()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }

    category<-category[1];

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    mdfr<-extractMDFR.Fisheries.CatchAbundance(objs,fleets=fleets,category=category,cast=cast,years=years,verbose=verbose);

    #----------------------------------
    #fishery catch abundance
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    if (verbose) cat("Fleets = ",uF,"\n");
    if (sum(grep('z',cast,fixed=TRUE))>0){
        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
        mdfr$z<-as.numeric(mdfr$z);
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
            subPlots<-list();
            mdfrp<-mdfr[mdfr$fleet==f,];
            uY<-sort(unique(mdfrp$y));
            uX<-sort(unique(mdfrp$x));
            uM<-sort(unique(mdfrp$m));
            uS<-sort(unique(mdfrp$s));
            for (x in uX){
                idx<-mdfrp$x==x;
                for (m in uM){
                    idm<-mdfrp$m==m;
                    for (s in uS){
                        ids<-mdfrp$s==s
                        for (pg in 1:ceiling(length(uY)/mxy)){
                            idy<-mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)];
                            mdfrpp<-mdfrp[idx&idm&ids&idy,];
                            if (nrow(mdfrpp)>0){
                                if (verbose) cat("Plotting ",x,m,s,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                                mdfrpp$y<-as.character(mdfrpp$y);
                                p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                                               facet_grid=facet_grid,scales=scales,
                                               facet_wrap=facet_wrap,nrow=nrow,
                                               xlab='size (mm CW)',ylab='Catch Abundance',units='millions',lnscale=lnscale,
                                               title=paste0(f," ",category," catch for \n",x," ",m," ",s),
                                               plotPoints=plotPoints,
                                               colour='case',guideTitleColor='',
                                               shape='case',guideTitleShape='',
                                               showPlot=FALSE);
                                if (showPlot||!is.null(pdf)) print(p);
                                cap<-paste0("\n  \nFigure &&figno. Predicted ",f," ",category," catch abundance for ",x," ",m," ",s,", (",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
                                subPlots[[cap]]<-p;
                            } else {
                                if (verbose) cat("Skipping ",x,m,s,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                            }
                        }#pg
                    }#uS
                }#uM
            }#uX
            plots[[f]]<-subPlots;
        }#uF
    } else {
        #plot time series
        if (verbose) cat("Plotting time series.\n")
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
            mdfrp<-mdfr[mdfr$fleet==f,];
            p<-plotMDFR.XY(mdfrp,x='y',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,scales=scales,
                           facet_wrap=facet_wrap,nrow=nrow,
                           xlab='year',ylab='Catch Abundance',units='millions',lnscale=lnscale,
                           title=paste0(f,"\n",category," catch"),
                           colour='case',guideTitleColor='',
                           shape='case',guideTitleShape='',
                           plotPoints=plotPoints,
                           colour_scale=colour_scale,
                           fill_scale=fill_scale,
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            cap<-paste0("\n  \nFigure &&figno. Predicted ",f," ",category," catch abundance.  \n  \n")
            plots[[cap]]<-p;
        }#uF
    }

    if (verbose) cat("rCompTCMs::compareResults.Fisheries.CatchAbundance: Done!\n");
    return(plots)
}
