#'
#'@title Compare recruitment size distributions among several model runs
#'
#'@description Function to compare recruitment size distributions among several model runs.
#'
#'@param objs - list of resLst objects (or a dataframe from [extractMDFR.Pop.RecSizeDistribution()])
#'@param scaleToDensity - flag to scale abundance to 1-mm size bins
#'@param aggToCutpts - flag to aggregate (rebin) to provided cutpts
#'@param cutpts - cutpoints to aggregate to
#'@param dodge - width to dodge overlapping series
#' @param plotPoints - flag to include points (default: FALSE)
#' @param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#' @param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details If scaleToDensity is true, the size distribution is scaled to abundance/mm to allow easier comparison between models
#'with different bin sizes. If aggToCutpts is true, the distribution(s) are re-binned (aggregated) to a common set of
#'cutpoints. If objs is a list of resLst objects, then [extractMDFR.Pop.RecSizeDistribution()] is called first to obtain the size
#'distributions.
#'
#'@return ggplot object
#'
#'@import ggplot2
#'@import wtsPlots
#'
#'@export
#'
compareResults.Pop.RecSizeDistribution<-function(objs,
                                                 scaleToDensity=FALSE,
                                                 aggToCutpts=FALSE,
                                                 cutpts=seq(25,185,5),
                                                 dodge=0.2,
                                                 plotPoints=FALSE,
                                                 colour_scale=NULL,
                                                 fill_scale=NULL,
                                                 showPlot=TRUE,
                                                 pdf=NULL,
                                                 verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareResults.Pop.RecSizeDistribution().\n");
    options(stringsAsFactors=FALSE);
    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    std_theme = wtsPlots::getStdTheme();

    if (inherits(objs,"data.frame")){
        mdfr = objs;
    } else {
        cases<-names(objs);
        mdfr<-extractMDFR.Pop.RecSizeDistribution(objs,
                                                  scaleToDensity=scaleToDensity,
                                                  aggToCutpts=aggToCutpts,
                                                  cutpts=cutpts,
                                                  verbose);
    }

    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes(x=z,y=val,colour=case));
    p <- p + geom_line(position=pd);
    if (plotPoints) p <- p + geom_point(data=mdfrpp);
    if (!is.null(colour_scale)) p = p + colour_scale;
    if (!is.null(fill_scale))   p = p + fill_scale;
    p <- p + ylim(c(0,NA));
    if (scaleToDensity)
        p <- p + labs(x="size (mm CW)",y="recruitment size distribution\n(relative abundance/mm)");
    if (!scaleToDensity)
        p <- p + labs(x="size (mm CW)",y="recruitment size distribution");
    p = p + std_theme;

    if (showPlot) print(p);

    if (verbose) message("--finished rCompTCMs::compareResults.Pop.RecSizeDistribution().\n");
    return(p);
}

