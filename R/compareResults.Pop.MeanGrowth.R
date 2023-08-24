#'
#'@title Compare mean post-molt size among several model runs
#'
#'@description Function to compare mean post-molt size among several model runs.
#'
#'@param objs - list of resLst objects
#'@param dodge - width to dodge overlapping series
#' @param plotPoints - flag to include points (default: FALSE)
#' @param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#' @param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
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
compareResults.Pop.MeanGrowth<-function(objs,
                                        dodge=0.2,
                                        plotPoints=FALSE,
                                        colour_scale=NULL,
                                        fill_scale=NULL,
                                        showPlot=FALSE,
                                        pdf=NULL,
                                        verbose=FALSE){
    options(stringsAsFactors=FALSE);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    cases<-names(objs);

    mdfr<-extractMDFR.Pop.MeanGrowth(objs,verbose=verbose)

    p<-plotPop.MeanGrowth(mdfr,
                          plotPoints=plotPoints,
                          colour_scale=colour_scale,
                          fill_scale=fill_scale,
                          dodge=dodge,
                          showPlot=showPlot,
                          verbose=verbose);
    return(p);
}
