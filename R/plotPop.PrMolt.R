#'
#'@title Function to plot annual molt probability for immature crab from several model runs
#'
#'@description Function to plot annual molt probability for immature crab from several model runs.
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
plotPop.PrMolt<-function(mdfr,
                        dodge=0.2,
                        xbnds=NULL,
                        showPlot=TRUE,
                        verbose=FALSE){
    #----------------------------------
    # plot annual molt probability for immature crab from
    #----------------------------------
    mdfr$z<-as.numeric(mdfr$z);#make sure z is numeric
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    p <- p + geom_point(position=pd);
    if (!is.null(mdfr$lci)&&any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
    p <- p + coord_cartesian(xlim=xbnds,ylim=c(0,1));
    p <- p + labs(x='size (mm CW)',y="pr(annual molt)");
    p <- p + ggtitle("pr(Annual Molt)");
    p <- p + facet_grid(x~.);
    if (showPlot) print(p);

    return(p);
}
