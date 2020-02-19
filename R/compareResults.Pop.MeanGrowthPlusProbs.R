#'
#'@title Compare mean and probability of post-molt size among several model runs
#'
#'@description Function to compare mean and probability of post-molt size among several model runs.
#'
#'@param objs - list of resLst objects
#'@param scale - scaling factor for probabilities
#'@param zbnds - 2-element vector indicating lower and upper bounds on model size bins
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details None.
#'
#'@return list of ggplot objects (plots by sex)
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.MeanGrowthPlusProbs<-function(objs,
                                                 scale=10,
                                                 zbnds=c(25,185),
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

    mdfrMnG<-extractMDFR.Pop.MeanGrowth(objs,verbose);
    mdfrPrG<-extractMDFR.Pop.GrowthMatrices(objs,verbose);

    plts<-plotPop.MeanGrowthPlusProbs(mdfrMnG,
                                      mdfrPrG,
                                      scale=scale,
                                      zbnds=zbnds,
                                      showPlot=showPlot,
                                      verbose=verbose);
    return(plts);
}
