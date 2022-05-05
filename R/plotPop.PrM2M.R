#'
#'@title Function to plot probability of molt-to-maturity for several model runs
#'
#'@description Function to plot probability of molt-to-maturity for several model runs.
#'
#'@param mdfr - melted dataframe
#'@param dodge - width to dodge overlapping series
#'@param xbnds - two-element vector with x-axis bound (or NULL)
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
plotPop.PrM2M<-function(mdfr,
                        dodge=0.2,
                        xbnds=NULL,
                        showPlot=TRUE,
                        verbose=FALSE){
    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));
    #----------------------------------
    # plot probability of molt-to-maturity
    #----------------------------------
    mdfr$z<-as.numeric(mdfr$z);#make sure z is numeric
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    p <- p + geom_point(position=pd);
    if (!is.null(mdfr$lci)&&any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
    if (!is.null(xbnds)) p <- p + coord_cartesian(xlim=xbnds);
    p <- p + labs(x='size (mm CW)',y="pr(molt-to-maturity)");
    p <- p + ggtitle("pr(Molt-to-Maturity)");
    p <- p + facet_grid(x~.) + std_theme;
    if (showPlot) print(p);

    return(p);
}
