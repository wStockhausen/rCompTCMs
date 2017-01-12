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
                                             verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::compareResults.Pop.GrowthMatrices().\n")
    #create pdf, if necessary
    if (!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    cases<-names(objs);
    
    mdfr<-extractMDFR.Pop.GrowthMatrices(objs,verbose);

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

    if (verbose) cat("Finished rCompTCMs::compareResults.Pop.GrowthMatrices().\n")
    return(p);
}
