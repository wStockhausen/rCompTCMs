#'
#'@title Compare growth data among several model scenarios
#'
#'@description Function to compare growth data among several model scenarios.
#'
#'@param objs - list of resLst objects
#'@param dodge - width to dodge overlapping series
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
compareData.GrowthData<-function(objs,
                                 dodge=0.2,
                                 verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        mdfr1<-NULL;
        obj<-objs[[case]];
        #if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.MeanGrowth(obj,verbose);
        #if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.MeanGrowth(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Data.GrowthData(obj,verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    
    if (is.null(mdfr)) {
        cat("\n \nNo growth data.\n \n")
        return(NULL);
    }
    
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.character(mdfr$y);

    pd<-position_dodge(width=dodge);
    datasets<-unique(mdfr$category);
    plots<-list();
    for (d in datasets){
        mdfrp<-mdfr[(mdfr$category==d),];
        dcs<-unique(mdfrp$case);
        #-------------------------------------------#
        #plot growth data and fits
        #-------------------------------------------#
        p <- ggplot(mdfrp,aes_string(x='z',y='val',colour='case',shape='case'));
        p <- p + geom_point();
        if (any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
        p <- p + geom_abline(slope=1,linetype=2);
        p <- p + labs(x='pre-molt size (mm CW)',y="post-molt size (mm CW)");
        p <- p + ggtitle(d);
        p <- p + facet_grid(x~.);
        cap<-paste0("\n  \nFigure &&figno. Model fits to ",d,".\n   \n")
        plots[[cap]]<-p;
    }#d

    return(plots);
}
