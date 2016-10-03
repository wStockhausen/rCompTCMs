#'
#'@title Compare growth matrices among several model runs
#'
#'@description Function to compare growth matrices among several model runs.
#'
#'@param objs - list of resLst objects
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details If multiple models are compared, then a set of sex-specific faceted line plots 
#'are created. If a single model is plotted, then sex-specific bubble plots are created.
#'
#'@return list of ggplot objects, by sex
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.GrowthMatrices<-function(objs,
                                             showPlot=FALSE,
                                             pdf=NULL,
                                             verbose=TRUE){

    #create pdf, if necessary
    if (!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    cases<-names(objs);
    
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.GrowthMatrices(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.GrowthMatrices(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.GrowthMatrices(obj,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-as.numeric(mdfr$z);
    mdfr$zp<-as.numeric(mdfr$zp);
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    #----------------------------------
    # plot growth transition matrices
    #----------------------------------
    p <- ggplot(mdfr,aes_string(y='zp',x='z',size='val',fill='val'));
    p <- p + geom_abline(slope=1,colour='black',linetype=2)
    p <- p + geom_point(alpha=0.8,shape=21) + scale_size_area(max_size=10)
    p <- p + scale_fill_gradientn(colours=wtsUtilities::createColorPalette('jet',100,alpha=0.7))
    p <- p + labs(x="pre-molt size (mm CW)", y="post-molt size (mm CW)");
    p <- p + guides(size=guide_legend("probability",order=1));
    p <- p + guides(fill=guide_colorbar("probability",alpha=1.0,order=2));
    if (length(cases)==1){
        p <- p + facet_grid(x~.);
    } else {
        p <- p + facet_grid(case~x);
    }
    if (showPlot) print(p);

    return(p);
}
