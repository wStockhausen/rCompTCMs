#'
#'@title Compare recruitment size distributions among several model runs
#'
#'@description Function to compare recruitment size distributions among several model runs.
#'
#'@param obj - list of resLst objects
#'@param dodge - width to dodge overlapping series
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
compareResults.Pop.RecSizeDistribution<-function(objs,
                                                 dodge=0.2,
                                                 showPlot=TRUE,
                                                 pdf=NULL,
                                                 verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::compareResults.Pop.RecSizeDistribution().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    mdfr<-extractMDFR.Pop.RecSizeDistribution(objs,verbose)

    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    p <- p + geom_point(position=pd);
    p <- p + ylim(c(0,NA));
    p <- p + labs(x="size (mm CW)",y="recruitment size distribution");

    if (showPlot) print(p);

    if (verbose) cat("--finished rCompTCMs::compareResults.Pop.RecSizeDistribution().\n");
    return(p);
}

