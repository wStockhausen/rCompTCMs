#'
#'@title Compare mean post-molt size among several model runs
#'
#'@description Function to compare mean post-molt size among several model runs.
#'
#'@param objs - list of resLst objects
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
compareResults.Pop.MeanGrowth<-function(objs,
                                        dodge=0.2,
                                        showPlot=FALSE,
                                        pdf=NULL,
                                        verbose=FALSE){
    options(stringsAsFactors=FALSE);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    cases<-names(objs);

    mdfr<-extractMDFR.Pop.MeanGrowth(objs,verbose)

    p<-plotPop.MeanGrowth(mdfr,
                          dodge=dodge,
                          showPlot=showPlot,
                          verbose=verbose);
    return(p);
}
