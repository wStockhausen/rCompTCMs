#'
#'@title Compare probability of molt-to-maturity among several model runs
#'
#'@description Function to compare probability of molt-to-maturity among several model runs.
#'
#'@param objs - list of resLst objects
#'@param dodge - width to dodge overlapping series
#'@param colour_scale - ggplot2 scale_colour object (default is [ggplot2::scale_colour_hue()])
#'@param fill_scale - ggplot2 scale_colour object (default is [ggplot2::scale_fill_hue()])
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
#'@md
#'
#'@export
#'
compareResults.Pop.PrM2M<-function(objs,
                                   dodge=0.2,
                                   plotPoints=FALSE,
                                   colour_scale=ggplot2::scale_color_hue(),
                                   fill_scale=ggplot2::scale_fill_hue(),
                                   showPlot=TRUE,
                                   pdf=NULL,
                                   verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    mdfr<-extractMDFR.Pop.PrM2M(objs,verbose=verbose);

    p<- plotPop.PrM2M(mdfr,
                      dodge=dodge,
                      showPlot=showPlot,
                      verbose=verbose);
    p<-p+colour_scale;
    return(p);
}
