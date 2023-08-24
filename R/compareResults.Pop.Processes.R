#'
#'@title Compare population processes from TCSAM2013 and TCSAM02 model runs
#'
#'@description Function to compare population processes from TCSAM2013 and TCSAM02 model runs.
#'
#'@param objs - list of resLst objects
#' @param plotPoints - flag to include points (default: FALSE)
#' @param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#' @param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print debug info
#'
#'@return list of ggplot2 objects.
#'
#'@details Uses \code{wtsUtilities::printGGList()}. List names are captions.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Processes<-function(objs,
                                       plotPoints=FALSE,
                                       colour_scale=NULL,
                                       fill_scale=NULL,
                                       showPlot=TRUE,
                                       pdf=NULL,
                                       width=8,
                                       height=6,
                                       verbose=FALSE){
    options(stringsAsFactors=FALSE);

    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    cases<-names(objs);

    plots<-list();#output list
    figno<-1;

    #-------------------------------------------#
    #plot natural mortality
    #-------------------------------------------#
    p<-compareResults.Pop.NaturalMortality(objs,
                                           dodge=0.2,
                                           plotPoints=plotPoints,
                                           colour_scale=colour_scale,
                                           fill_scale=fill_scale,
                                           showPlot=FALSE,
                                           verbose=verbose);
    cap<-"  \n  \nFigure &&fno.Estimated natural mortality rates.  \n  \n";
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot prM2M
    #-------------------------------------------#
    p<-compareResults.Pop.PrM2M(objs,
                                dodge=0.5,
                                plotPoints=plotPoints,
                                colour_scale=colour_scale,
                                fill_scale=fill_scale,
                                showPlot=FALSE,
                                verbose=verbose);
    cap<-"  \n  \nFigure &&fno. Estimated probabilities of molt-to-maturity at size (mm CW).  \n  \n";
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot mean growth
    #-------------------------------------------#
    p<-compareResults.Pop.MeanGrowth(objs,
                                     dodge=0.5,
                                     plotPoints=plotPoints,
                                     colour_scale=colour_scale,
                                     fill_scale=fill_scale,
                                     showPlot=FALSE,
                                     verbose=verbose);
    cap<-"  \n  \nFigure &&fno. Estimated mean growth patterns.  \n  \n";
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot growth transition matrices
    #-------------------------------------------#
    p<-compareResults.Pop.GrowthMatrices(objs,showPlot=FALSE,verbose=verbose);
    cap<-paste0("  \n  \nFigure &&fno. Estimated growth transition matrices.  \n  \n");
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    p<-compareResults.Pop.RecSizeDistribution(objs,
                                              dodge=0.5,
                                               plotPoints=plotPoints,
                                               colour_scale=colour_scale,
                                               fill_scale=fill_scale,
                                              showPlot=FALSE,
                                              verbose=verbose);
    cap<-paste0("  \n  \nFigure &&fno. Estimated/assumed size distribution at recruitment to the model.  \n  \n");
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    return(invisible(plots));
}
