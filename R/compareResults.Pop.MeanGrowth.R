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

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.MeanGrowth(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.MeanGrowth(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.MeanGrowth(obj,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    #-------------------------------------------#
    #plot mean growth
    #-------------------------------------------#
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    p <- p + geom_point(position=pd);
    if (any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
    p <- p + geom_abline(slope=1,linetype=2);
    p <- p + labs(x='pre-molt size (mm CW)',y="post-molt size (mm CW)");
    p <- p + ggtitle("Mean Growth");
    p <- p + facet_grid(x~.);
    if (showPlot) print(p);

    return(p);
}
