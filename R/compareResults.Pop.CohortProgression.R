#'
#'@title Function to compare cohort progression by year among several models
#'
#'@description This function compares cohort progression by year
#'   among several models.
#'
#'@param objs - list of TCSAM02 resLst objects (or a dataframe from [extractMDFR.Pop.CohortProgression()])
#'@param cast - casting formula for excluding x,m,s,z factor levels from sums across the unspecified factors
#'@param scaleToDensity - flag to scale abundance to 1-mm size bins
#'@param aggToCutpts - flag to aggregate (rebin) to provided cutpts
#'@param cutpts - cutpoints to aggregate to
#'@param facet_grid - formula for faceting using facet_grid
#'@param facet_wrap - formula for faceting using facet_wrap
#'@param dodge - width to dodge overlapping series
#'@param years - numeric vector of years to include (or NULL for all)
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing
#'@param lnscale - use log scale on y axis
#'@param scales - scales parameter for facet_grid/facet_wrap
#'@param types - types of plots to produce ("progression","byyear")
#'@param plotPoints - flag to plot points
#'@param shapes - name of column for ggplot shape factors (defaults to 's' for shell condition)
#'@param plotLines - flag to plot cohorts as lines
#'@param linetypes - name of column for ggplot linetype factors (defaults to 'ms' for maturity + shell condition)
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - filename for pdf output (or NULL for no pdf)
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return a list of ggplot2 objects, with potential captions as element names.
#'
#'@details If \code{objs} is a list of TCSAM02 resLst objects,
#'model results are extracted using [extractMDFR.Pop.CohortProgression()].
#'\code{objs} can also be a dataframe output [extractMDFR.Pop.CohortProgression()].
#'Plots are made using [plotMDFR.XY()].
#'You can an plot cohort abundance as a progression by stage for each model (\code{types="progression"} or
#'as size distributions by year with colours indicating different models (\code{types="byyear"}.
#'Sex-specific cohort progressions are plotted separately. By default,
#'different maturity/shell condition categories are indicated using different linetypes.
#'
#'@import ggplot2

#'@export
#'
compareResults.Pop.CohortProgression<-function(objs,
                                       cast="x+m+s+z",
                                       scaleToDensity=FALSE,
                                       aggToCutpts=FALSE,
                                       cutpts=seq(25,185,5),
                                       facet_grid=NULL,
                                       facet_wrap=NULL,
                                       dodge=0.2,
                                       years=NULL,
                                       mxy=4,
                                       nrow=2,
                                       lnscale=FALSE,
                                       scales="fixed",
                                       types=c("progression","byyear"),
                                       plotPoints=FALSE,
                                       shapes="s",
                                       plotLines=TRUE,
                                       linetypes="ms",
                                       showPlot=FALSE,
                                       pdf=NULL,
                                       verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::compareResults.Pop.CohortProgression().\n");
    options(stringsAsFactors=FALSE);

    if (is.null(cast)){
        cat("Error in rCompTCMs::compareResults.Pop.CohortProgression()\n");
        cat("Must supply a 'cast' formula!\nReturning NULL.\n");
        return(NULL);
    }

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }


    if (inherits(objs,"data.frame")){
        mdfr = objs;
    } else {
        mdfr<-extractMDFR.Pop.CohortProgression(objs,
                                                cast=cast,
                                                scaleToDensity=scaleToDensity,
                                                aggToCutpts=aggToCutpts,
                                                cutpts=cutpts,
                                                years=years,
                                                verbose=verbose);
    }

    # plots<-plotPop.CohortProgression(mdfr,
    #                                  facet_grid=facet_grid,
    #                                  facet_wrap=facet_wrap,
    #                                  dodge=dodge,
    #                                  years=years,
    #                                  mxy=mxy,
    #                                  nrow=nrow,
    #                                  lnscale=lnscale,
    #                                  scales=scales,
    #                                  types=types,
    #                                  plotPoints=FALSE,
    #                                  shapes="s",
    #                                  plotLines=TRUE,
    #                                  linetypes="ms",
    #                                  showPlot=showPlot,
    #                                  verbose=verbose);
    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));
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
        if (verbose) message("Plotting progression\n");
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
        if (verbose) message("Plotting progression by year\n")
        mdfrp$ms = paste(mdfrp$m,mdfrp$s);
        # uM<-sort(unique(mdfrp$m));
        # uS<-sort(unique(mdfrp$s));
        uMS<-sort(unique(mdfrp$ms));
        for (x in uX){
            idx<-mdfrp$x==x;
            for (pg in 1:ceiling(length(uY)/mxy)){
                idy<-as.numeric(mdfrp$y) %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)];
                mdfrpp<-mdfrp[idx&idy,];
                if (nrow(mdfrpp)>0){
                    if (verbose) message("Plotting ",x,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                    mdfrpp$y<-factor(as.character(mdfrpp$y),levels=as.character(uY));
                    p<-plotMDFR.XY(mdfrpp,
                                   x='z',value.var='val',
                                   agg.formula=NULL,
                                   facet_wrap=~y,nrow=nrow,scales=scales,
                                   xlab='size (mm CW)',ylab='Cohort Abundance',
                                   units='millions',lnscale=lnscale,
                                   title=paste0(x),
                                   colour='case',guideTitleColor='',
                                   plotPoints=FALSE,
                                   shape=shapes,guideTitleShape='',
                                   plotLines=TRUE,
                                   linetype=linetypes,guideTitleLineType='',
                                   showPlot=FALSE);
                    if (showPlot) print(p);
                    cap<-paste0("\n  \nFigure &&figno. Cohort progression size comps for ",x,", (",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
                    plots[[cap]]<-p;
                } else {
                    if (verbose) message("Skipping ",x,paste0(uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],collapse=','),"\n");
                }
            }#pg
        }#uX
    }

    if (verbose) cat("finished rCompTCMs::compareResults.Pop.CohortProgression().\n");
    return(plots)
}
