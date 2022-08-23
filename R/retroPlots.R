#'
#'@title Create plots of several OFL-related quantities from a retrospective analysis
#'
#'@description This function create plots for several OFL-related quantities from a retrospective analysis.
#'
#'@param dfr.OFLs - dataframe of OFL-related values from a retrospective analysis
#'@param x - name of dataframe column with "peel"labels
#'@param xlab - label for x-axis
#'
#'@return list of ggplot2 objects
#'
#'@details Creates bar plots for average recruitment, Bmsy, Fmsy, terminal year MMB,
#'projected MMB, and status
#'
#'@export
#'
retroPlotOFLResults<-function(dfr.OFLs,x="case",xlab="Peel"){
  plots<-list();
  p <- plotMDFR.Bars(dfr.OFLs,x=x,value.var="avgRec",colour="case",fill="case",
                     xlab=xlab,ylab="Average Recruitment (millions)");
  plots[["Average Recruitment (millions)"]]<-p;

  p <- plotMDFR.Bars(dfr.OFLs,x=x,value.var="Bmsy",colour="case",fill="case",
                     xlab=xlab,ylab="Bmsy (1000's t)");
  plots[["Bmsy (1000's t)"]]<-p;

  p <- plotMDFR.Bars(dfr.OFLs,x=x,value.var="Fmsy",colour="case",fill="case",
                     xlab=xlab,ylab="Fmsy (per year)");
  plots[["Fmsy (per year)"]]<-p;

  p <- plotMDFR.Bars(dfr.OFLs,x=x,value.var="curB",colour="case",fill="case",
                     xlab=xlab,ylab="Terminal MMB (1000's t)");
  plots[["Terminal MMB (1000's t)"]]<-p;

  p <- plotMDFR.Bars(dfr.OFLs,x=x,value.var="prjB",colour="case",fill="case",
                     xlab=xlab,ylab="Projected MMB (1000's t)");
  plots[["Projected MMB (1000's t)"]]<-p;

  p <- plotMDFR.Bars(dfr.OFLs,x=x,value.var="status",colour="case",fill="case",
                     xlab=xlab,ylab="status (projected MMB/Bmsy)");
  p <- p + ggplot2::geom_hline(yintercept=1.0,linetype=2,colour="dark blue")
  p <- p + ggplot2::geom_hline(yintercept=0.5,linetype=3,colour="red")
  plots[["status (projected MMB/Bmsy)"]]<-p;

  p <- plotMDFR.Bars(dfr.OFLs,x=x,value.var="OFL",colour="case",fill="case",
                     xlab=xlab,ylab="OFL (1,000's t)");
  plots[["OFL (1,000's t)"]]<-p;

  return(plots)
}


#'
#'@title Create a plot of time series from a retrospective analysis
#'
#'@description This function creates a plot of time series from a retrospective analysis.
#'
#'@param dfr - dataframe of estimated values extracted from a series of retrospective model runs
#'@param ylab - label for y-axis
#'@param units - label for y-axis units
#'@param lnscale - flag to use log scale
#'@param dodge - amount to dodge overlapping values
#'@param size - point shape size
#'@param colour - column name indicating retrospective model (default='case')
#'@param shape - column name for shapes (default=value assigned to colour)
#'@param verbose - flag
#'
#'@return a ggplot2 object
#'
#'@details Uses \code{rCompTCMs::plotMDFR.XY} to plot the timeseries.
#'
#'@export
#'
retroPlotTimeSeries<-function(dfr,ylab,units,lnscale=FALSE,dodge=0,size=2,
                              colour="case",shape=colour,verbose=FALSE){
  if (verbose) cat("colour = '",colour,"'\n","shape = '",shape,"'\n",sep="");
  p<-rCompTCMs::plotMDFR.XY(dfr,
                            x='y',value.var="val",agg.formula=NULL,
                            xlab='year',ylab=ylab,units=units,lnscale=lnscale,
                            dodge=dodge,size=size,
                            colour=colour,guideTitleColour=ifelse(colour=="case",'peel',""),
                            shape=shape,guideTitleShape=ifelse(shape=="case",'peel',""));
  return(p)
}

#'
#'@title Create a plot of model fits to time series from a retrospective analysis
#'
#'@description This function creates a plot of model fits to time series from a retrospective analysis.
#'
#'@param dfr - dataframe of estimated values extracted from a series of retrospective model runs
#'@param ylab - label for y-axis
#'@param units - label for y-axis units
#'@param lnscale - flag to use log scale
#'@param dodge - amount to dodge overlapping values
#'@param size - point shape size
#'@param colour - column name indicating retrospective model (default='case')
#'@param shape - column name for shapes (default=value assigned to colour)
#'@param facet_grid - formula to apply faceting
#'@param verbose - flag
#'
#'@return a ggplot2 object
#'
#'@details Uses \code{rCompTCMs::plotMDFR.XY} to plot the timeseries.
#'
#'@export
#'
retroPlotFitsAsTimeSeries<-function(dfr,ylab,units,lnscale=FALSE,dodge=0,size=2,
                                   colour="case",shape=colour,
                                   facet_grid=termyear~.,
                                   verbose=FALSE){
  if (verbose) cat("colour = '",colour,"'\n","shape = '",shape,"'\n",sep="");
  p<-rCompTCMs::plotMDFR.XY(dfr,
                            x='y',value.var="val",agg.formula=NULL,facet_grid=facet_grid,
                            xlab='year',ylab=ylab,units=units,lnscale=lnscale,
                            dodge=dodge,size=size,
                            colour=colour,guideTitleColour='',
                            shape=shape,guideTitleShape='');
  return(p)
}

#'
#'@title Create a plot of values from a retrospective analysis as bars
#'
#'@description This function creates a plot of values from a retrospective analysis as bars.
#'
#'@param dfr - dataframe of estimated values extracted from a series of retrospective model runs
#'@param xlab - label for x-axis
#'@param ylab - label for y-axis
#'@param facet_grid - formula to apply faceting
#'
#'@return a ggplot2 object
#'
#'@details The column "case" should indicate the peels of the retrospective runs
#'
#'@import ggplot2
#'@importFrom wtsPlots getStdTheme
#'
#'@export
#'
retroPlotValuesAsBars<-function(dfr,xlab="peel",ylab="",facet_grid=NULL){
  p<-ggplot(data=dfr,mapping=aes_string(x="case",weight="val",fill="case"));
  p<-p+geom_bar(colour=NA,position=position_dodge());
  p<-p+labs(x=xlab,y=ylab,fill=xlab);
  if (!is.null(facet_grid)) p<-p+facet_grid(facet_grid);
  p<-p+wtsPlots::getStdTheme();
  return(p)
}

#'
#'@title Create a plot of values across size from a retrospective analysis
#'
#'@description This function creates a plot of values across size from a retrospective analysis.
#'
#'@param dfr - dataframe of estimated values across size extracted from a series of retrospective model runs
#'@param xlab - label for x-axis
#'@param ylab - label for y-axis
#'@param facet_grid - formula to apply faceting
#'
#'@return a ggplot2 object
#'
#'@details The column "case" should indicate the peels of the retrospective runs
#'
#'@import ggplot2
#'@importFrom wtsPlots getStdTheme
#'
#'@export
#'
retroPlotValuesBySize<-function(dfr,xlab="size (mm CW)",ylab="",facet_grid=NULL){
  p<-ggplot(data=dfr,mapping=aes_string(x="z",y="val",colour="case",shape="case"));
  p<-p+geom_line()+geom_point();
  p<-p+labs(x=xlab,y=ylab,colour="peel",shape="peel")
  if (!is.null(facet_grid)) p<-p+facet_grid(facet_grid);
  p<-p+wtsPlots::getStdTheme();
  return(p)
}

