#'
#'@title Compare probability of molt-to-maturity among several model runs
#'
#'@description Function to compare probability of molt-to-maturity among several model runs.
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
compareResults.Pop.PrM2M<-function(objs,
                                   dodge=0.2,
                                   showPlot=TRUE,
                                   pdf=NULL,
                                   verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    mdfr<-extractMDFR.Pop.PrM2M(objs,verbose);
    
    #----------------------------------
    # plot probability of molt-to-maturity
    #----------------------------------
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    p <- p + geom_point(position=pd);
    if (any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
    p <- p + labs(x='size (mm CW)',y="pr(molt-to-maturity)");
    p <- p + ggtitle("pr(Molt-to-Maturity)");
    p <- p + facet_grid(x~.);
    if (showPlot) print(p);

    return(p);
}
