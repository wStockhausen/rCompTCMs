#'
#'@title Plot mean post-molt size for several model runs
#'
#'@description Function to plot mean post-molt size for several model runs.
#'
#'@param mdfr - melted dataframe
#'@param dodge - width to dodge overlapping series
#' @param plotPoints - flag to include points (default: FALSE)
#' @param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#' @param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param showPlot - flag to print plot to current device
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
plotPop.MeanGrowth<-function(mdfr,
                            dodge=0.2,
                            plotPoints=FALSE,
                            colour_scale=NULL,
                            fill_scale=NULL,
                            showPlot=FALSE,
                            verbose=FALSE){
    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));
    #-------------------------------------------#
    #plot mean growth
    #-------------------------------------------#
    mdfr$z<-as.numeric(mdfr$z);
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    if (plotPoints) p <- p + geom_point(data=mdfrpp);
    if (!is.null(colour_scale)) p = p + colour_scale;
    if (!is.null(fill_scale))   p = p + fill_scale;
    if ((!is.null(mdfr$lci))&&any(!is.na(mdfr$lci)))
        p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
    p <- p + geom_abline(slope=1,linetype=2);
    p <- p + labs(x='pre-molt size (mm CW)',y="post-molt size (mm CW)");
#    p <- p + ggtitle("Mean Growth");
    p <- p + facet_grid(x~.) + std_theme;
    if (showPlot) print(p);

    return(p);
}
