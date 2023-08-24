#'
#'@title Plot comparisons of fits to time series from a set of model runs, with a "zoom" for recent years
#'
#'@description Function to plot a comparison of fits to time series from a set of model runs, with a "zoom" for recent years.
#'
#'@param dfr - dataframe
#'@param plot1stObs - flag to plot observations from first case only
#'@param numRecent - number of "recent" years to plot
#'@param x - column name with x axis values
#'@param y - column name with y axis values
#'@param lci - column name with y axis values
#'@param uci - column name with y axis values
#'@param case - column name with case names
#'@param type - column name with type values (i.e., "observed","predicted")
#'@param facets - grid faceting formula (as an expresssion for multilevel faceting)
#'@param scales - ggplot2 scales option for facet_grid
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param plotObs - plot observations
#'@param plotMod - plot model fits/predictions
#'@param xlab -
#'@param ylab -
#'@param title -
#'@param xlims -
#'@param ylims -
#'@param colour_scale - ggplot2 scale_colour object (default is ggplot2::scale_colour_hue())
#'@param fill_scale - ggplot2 scale_fill object (default is ggplot2::scale_fill_hue())
#'@param showPlot - flag (T/F) to print plot
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@details Uses [plotMDFR.Fits.TimeSeries1()] to make the plots. \code{numRecent} provides the "zoom"
#'for a second set of faceted plots including only the most recent years. If \code{numRecent} is 0,
#'this plot is not created and an NA is substituted.
#'
#'Note that \code{facets} should be given as an expression, not as a character string, if you want multilevel faceting.
#'
#'@return list with requested ggplot objects
#'
#' @md
#'
#'@export
#'
plotMDFR.Fits.TimeSeries<-function(dfr,
                                  plot1stObs=TRUE,
                                  numRecent=15,
                                  x="y",
                                  y="val",
                                  lci="lci",
                                  uci="uci",
                                  case="case",
                                  type="type",
                                  facets=NULL,
                                  scales="fixed",
                                  position=ggplot2::position_dodge(0.2),
                                  plotObs=TRUE,
                                  plotMod=TRUE,
                                  xlab='year',
                                  ylab=NULL,
                                  title=NULL,
                                  xlims=NULL,
                                  ylims=NULL,
                                  colour_scale=ggplot2::scale_colour_hue(),
                                  fill_scale=ggplot2::scale_fill_hue(),
                                  showPlot=FALSE,
                                  verbose=FALSE){
    plots<-list();

    #plot with observations & case results
    if (plotObs&&plotMod){
        if (verbose) message("plotMDFR.Fits.TimeSeries: plot with observations & case results");
        p1<-plotMDFR.Fits.TimeSeries1(dfr,
                                      plot1stObs=plot1stObs,
                                      x=x,
                                      y=y,
                                      lci=lci,
                                      uci=uci,
                                      case=case,
                                      type=type,
                                      facets=facets,
                                      scales=scales,
                                      position=position,
                                      plotObs=TRUE,
                                      plotMod=TRUE,
                                      xlab=xlab,
                                      ylab=ylab,
                                      title=title,
                                      xlims=xlims,
                                      ylims=ylims,
                                      colour_scale=colour_scale,
                                      fill_scale=fill_scale,
                                      showPlot=showPlot,
                                      verbose=verbose);
        plots$p1<-p1;

        #plot only over time period with observations
        if (verbose) message("plotMDFR.Fits.TimeSeries: plot only over time period with only observations")
        idx<-dfr[[type]]=='observed';
        xplims<-range(dfr[[x]][idx],na.rm=TRUE);
        if (!is.null(xlims)){
            xplims[1]<-max(xlims[1],xplims[1],na.rm=TRUE);#max of mins
            xplims[2]<-min(xlims[2],xplims[2],na.rm=TRUE);#min of maxes
        }
        yplims<-NULL;
        if (!is.null(ylims)){
            idy<-dfr[[x]] %in% xplims[1]:xplims[2];
            yplims<-range(dfr[[y]][idy],na.rm=TRUE,finite=TRUE);
            yplims[1]<-max(ylims[1],yplims[1],na.rm=TRUE);#max of mins
            yplims[2]<-min(ylims[2],yplims[2],na.rm=TRUE);#min of maxes
        }
        dfrp<-dfr[(dfr[[x]]>=xplims[1])&(dfr[[x]]<=xplims[2]),];
        p2<-plotMDFR.Fits.TimeSeries1(dfrp,
                                      plot1stObs=plot1stObs,
                                      x=x,
                                      y=y,
                                      lci=lci,
                                      uci=uci,
                                      case=case,
                                      type=type,
                                      facets=facets,
                                      scales=scales,
                                      position=position,
                                      plotObs=TRUE,
                                      plotMod=TRUE,
                                      xlab=xlab,
                                      ylab=ylab,
                                      title=title,
                                      xlims=xplims,
                                      ylims=yplims,
                                      colour_scale=colour_scale,
                                      fill_scale=fill_scale,
                                      showPlot=showPlot,
                                      verbose=verbose);
        plots$p2<-p2;

        #plot in recent years only
        if (verbose) message("plotMDFR.Fits.TimeSeries: plot in recent years only")
        if (numRecent>0){
            xmx<-max(dfr[[x]],na.rm=TRUE);
            xplims<-c(xmx-numRecent,xmx+1);
            if (!is.null(xlims)){
                xplims[1]<-max(xlims[1],xplims[1],na.rm=TRUE);#max of mins
                xplims[2]<-min(xlims[2],xplims[2],na.rm=TRUE);#min of maxes
            }
            yplims<-NULL;
            if (!is.null(ylims)){
                idy<-dfr[[x]] %in% xplims[1]:xplims[2];
                yplims<-range(dfr[[y]][idy],na.rm=TRUE,finite=TRUE);
                yplims[1]<-max(ylims[1],yplims[1],na.rm=TRUE);#max of mins
                yplims[2]<-min(ylims[2],yplims[2],na.rm=TRUE);#min of maxes
            }
            dfrp<-dfr[dfr[[x]]>=(xmx-numRecent),];
            p3<-plotMDFR.Fits.TimeSeries1(dfrp,
                                          plot1stObs=plot1stObs,
                                          x=x,
                                          y=y,
                                          lci=lci,
                                          uci=uci,
                                          case=case,
                                          type=type,
                                          facets=facets,
                                          scales=scales,
                                          position=position,
                                          plotObs=TRUE,
                                          plotMod=TRUE,
                                          xlab=xlab,
                                          ylab=ylab,
                                          title=title,
                                          xlims=xplims,
                                          ylims=yplims,
                                          colour_scale=colour_scale,
                                          fill_scale=fill_scale,
                                          showPlot=showPlot,
                                          verbose=verbose);
            plots$p3<-p3;
        } else {plots$p3=NA;}
    }

    #plot with observations only
    if (plotObs&&(!plotMod)){
        if (verbose) message("plotMDFR.Fits.TimeSeries: plot with observations only")
        p1<-plotMDFR.Fits.TimeSeries1(dfr,
                                      plot1stObs=plot1stObs,
                                      x=x,
                                      y=y,
                                      lci=lci,
                                      uci=uci,
                                      case=case,
                                      type=type,
                                      facets=facets,
                                      scales=scales,
                                      position=position,
                                      plotObs=TRUE,
                                      plotMod=FALSE,
                                      xlab=xlab,
                                      ylab=ylab,
                                      title=title,
                                      xlims=xlims,
                                      ylims=ylims,
                                      colour_scale=colour_scale,
                                      fill_scale=fill_scale,
                                      showPlot=showPlot,
                                      verbose=verbose);
        plots$p1<-p1;
        #plot in recent years only
        if (numRecent>0){
            xmx<-max(dfr[[x]],na.rm=TRUE);
            xplims<-c(xmx-numRecent,xmx+1);
            if (!is.null(xlims)){
                xplims[1]<-max(xlims[1],xplims[1],na.rm=TRUE);#max of mins
                xplims[2]<-min(xlims[2],xplims[2],na.rm=TRUE);#min of maxes
            }
            yplims<-NULL;
            if (!is.null(ylims)){
                idy<-dfr[[x]] %in% xplims[1]:xplims[2];
                yplims<-range(dfr[[y]][idy],na.rm=TRUE,finite=TRUE);
                yplims[1]<-max(ylims[1],yplims[1],na.rm=TRUE);#max of mins
                yplims[2]<-min(ylims[2],yplims[2],na.rm=TRUE);#min of maxes
            }
            dfrp<-dfr[dfr[[x]]>=(xmx-numRecent),];
            p2<-plotMDFR.Fits.TimeSeries1(dfrp,
                                          plot1stObs=plot1stObs,
                                          x=x,
                                          y=y,
                                          lci=lci,
                                          uci=uci,
                                          case=case,
                                          type=type,
                                          facets=facets,
                                          scales=scales,
                                          position=position,
                                          plotObs=TRUE,
                                          plotMod=FALSE,
                                          xlab=xlab,
                                          ylab=ylab,
                                          title=title,
                                          xlims=xplims,
                                          ylims=yplims,
                                          colour_scale=colour_scale,
                                          fill_scale=fill_scale,
                                          showPlot=showPlot,
                                          verbose=verbose);
            plots$p2<-p2;
        } else {plots$p2=NA;}
    }

    #plot with case results only
    if (plotMod&&(!plotObs)){
        if (verbose) message("plotMDFR.Fits.TimeSeries: plot with case results only")
        #plot full time series
        p1<-plotMDFR.Fits.TimeSeries1(dfr,
                                      plot1stObs=plot1stObs,
                                      x=x,
                                      y=y,
                                      lci=lci,
                                      uci=uci,
                                      case=case,
                                      type=type,
                                      facets=facets,
                                      scales=scales,
                                      position=position,
                                      plotObs=FALSE,
                                      plotMod=TRUE,
                                      xlab=xlab,
                                      ylab=ylab,
                                      title=title,
                                      xlims=xlims,
                                      ylims=ylims,
                                      colour_scale=colour_scale,
                                      fill_scale=fill_scale,
                                      showPlot=showPlot,
                                      verbose=verbose);
        plots$p1<-p1;
        #plot in recent years only
        if (numRecent>0){
            xmx<-max(dfr[[x]],na.rm=TRUE);
            xplims<-c(xmx-numRecent,xmx+1);
            if (!is.null(xlims)){
                xplims[1]<-max(xlims[1],xplims[1],na.rm=TRUE);#max of mins
                xplims[2]<-min(xlims[2],xplims[2],na.rm=TRUE);#min of maxes
            }
            yplims<-NULL;
            if (!is.null(ylims)){
                idy<-dfr[[x]] %in% xplims[1]:xplims[2];
                yplims<-range(dfr[[y]][idy],na.rm=TRUE,finite=TRUE);
                yplims[1]<-max(ylims[1],yplims[1],na.rm=TRUE);#max of mins
                yplims[2]<-min(ylims[2],yplims[2],na.rm=TRUE);#min of maxes
            }
            dfrp<-dfr[dfr[[x]]>=(xmx-numRecent),];
            p2<-plotMDFR.Fits.TimeSeries1(dfrp,
                                          plot1stObs=plot1stObs,
                                          x=x,
                                          y=y,
                                          lci=lci,
                                          uci=uci,
                                          case=case,
                                          type=type,
                                          facets=facets,
                                          scales=scales,
                                          position=position,
                                          plotObs=FALSE,
                                          plotMod=TRUE,
                                          xlab=xlab,
                                          ylab=ylab,
                                          title=title,
                                          xlims=xplims,
                                          ylims=yplims,
                                          colour_scale=colour_scale,
                                          fill_scale=fill_scale,
                                          showPlot=showPlot,
                                          verbose=verbose);
            plots$p2<-p2;
        } else {plots$p2=NA;}
    }
    return(plots);
}
