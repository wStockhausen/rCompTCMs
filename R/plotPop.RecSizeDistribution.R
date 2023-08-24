#'
#'@title Plot recruitment size distributions among several model runs
#'
#'@description Function to plot recruitment size distributions among several model runs.
#'
#'@param mdfr - melted dataframe
#'@param dodge - width to dodge overlapping series
#'@param xbnds - two-element vector with x-axis bound (or NULL)
#'@param plotPoints - flag to include points on plot
#'@param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#'@param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details None.
#'
#'@return ggplot object
#'
#'@import ggplot2
#'
#'@export
#'
plotPop.RecSizeDistribution<-function(mdfr,
                                      dodge=0.2,
                                      xbnds=NULL,
                                       plotPoints=plotPoints,
                                       colour_scale=colour_scale,
                                       fill_scale=fill_scale,
                                      showPlot=TRUE,
                                      pdf=NULL,
                                      verbose=FALSE){

    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));
    if (verbose) cat("Starting rCompTCMs::plotPop.RecSizeDistribution().\n");
    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    if (plotPoints) p <- p + geom_point(position=pd);
    if (!is.null(colour_scale)) p = p + colour_scale;
    if (!is.null(fill_scale))   p = p + fill_scale;
    p <- p + coord_cartesian(xlim=xbnds,ylim=c(0,1));
    p <- p + labs(x="size (mm CW)",y="recruitment size distribution");
    p = p + std_theme;

    if (showPlot) print(p);

    if (verbose) cat("--finished rCompTCMs::plotPop.RecSizeDistribution().\n");
    return(p);
}

