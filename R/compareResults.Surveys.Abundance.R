#'
#'@title Function to compare estimated survey abundance by year among several models
#'
#'@description This function compares estimated survey abundance by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param fleets - vector of fleets to plot, or "all"
#'@param cast - casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
#'@param years - 'all' or vector of years to include
#'@param facet_grid - formula for faceting using facet_grid
#'@param facet_wrap - formula for faceting using facet_wrap
#'@param scales - parameter passed to ggplot2::facet_grid()
#'@param dodge - width to dodge overlapping series
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing
#'@param lnscale - use log scale on y axis
#'@param plotPoints - flag to include points (default: FALSE)
#'@param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#'@param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
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
compareResults.Surveys.Abundance<-function(objs,
                                           fleets="all",
                                           cast="x",
                                           years='all',
                                           facet_grid=NULL,
                                           facet_wrap=NULL,
                                           scales='fixed',
                                           dodge=0.2,
                                           mxy=15,
                                           nrow=5,
                                           lnscale=FALSE,
                                            plotPoints=FALSE,
                                            colour_scale=NULL,
                                            fill_scale=NULL,
                                           showPlot=FALSE,
                                           pdf=NULL,
                                           verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Surveys.Abundance().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(cast)){
        cat("Error in rCompTCMs::compareResults.Surveys.Abundance()\n");
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

    mdfr<-extractMDFR.Surveys.Abundance(objs,fleets=fleets,cast=cast,years=years,verbose=verbose);

    #----------------------------------
    #survey abundance
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    if (verbose) cat("Fleets:",uF,"\n");
    if (sum(grep('z',cast,fixed=TRUE))>0){
        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
        mdfr$z<-as.numeric(mdfr$z);
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
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
                                               facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,scales=scales,
                                               xlab='size (mm CW)',ylab='Survey Abundance',units='millions',lnscale=lnscale,
                                               title=f,
                                               plotPoints=plotPoints,
                                               colour_scale=colour_scale,
                                               fill_scale=fill_scale,
                                               colour='case',guideTitleColor='',
                                               shape='case',guideTitleShape='',
                                               showPlot=FALSE);
                                if (showPlot||!is.null(pdf)) print(p);
                                cap<-paste0("\n  \nFigure &&figno. ",f," catch abundance for ",x," ",m," ",s,", (",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
                                plots[[cap]]<-p;
                            } else {
                                if (verbose) cat("Skipping ",x,m,s,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                            }
                        }#pg
                    }#uS
                }#uM
            }#uX
        }#uF
    } else {
        #plot time series
        if (verbose) cat("Plotting time series.\n")
        for (f in uF){
            if (verbose) cat("Plotting fleet",f,"\n")
            mdfrp<-mdfr[mdfr$fleet==f,];
            p<-plotMDFR.XY(mdfrp,x='y',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,scales=scales,
                           xlab='year',ylab='Survey Abundance',units='millions',lnscale=lnscale,
                           title=f,
                           plotPoints=plotPoints,
                           colour_scale=colour_scale,
                           fill_scale=fill_scale,
                           colour='case',guideTitleColor='',
                           shape='case',guideTitleShape='',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            cap<-paste0("\n  \nFigure &&figno. ",f," catch abundance.  \n  \n")
            plots[[cap]]<-p;
        }#uF
    }

    if (verbose) cat("rCompTCMs::compareResults.Surveys.Abundance: Done!\n");
    return(plots)
}
