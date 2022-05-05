#'
#'@title Compare growth matrices among several model runs using bubble plots
#'
#'@description Function to compare growth matrices among several model runs using bubble plots.
#'
#'@param objs - list of resLst objects
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details If multiple models are compared, then a set of bubble plots
#'are created faceted by mode case (row) and sex (column).
#'If a single model is plotted, then bubble plots are created faceted by sex (row). One ggplot
#'object is returned as a named list, with the figure caption as the name.
#'
#'@return list with a ggplot object
#'
#'@import ggplot2
#'@import wtsPlots
#'
#'@export
#'
compareResults.Pop.GrowthMatrices<-function(objs,
                                             showPlot=FALSE,
                                             pdf=NULL,
                                             verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareResults.Pop.GrowthMatrices().\n")
    std_theme = wtsPlots::getStdTheme();

    #create pdf, if necessary
    if (!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    cases<-names(objs);

    mdfr<-extractMDFR.Pop.GrowthMatrices(objs,verbose=verbose);

    #----------------------------------
    # plot growth transition matrices
    #----------------------------------
    mx<-max(mdfr$val,na.rm=TRUE);
    plots<-list();
    for (case in cases){
        mdfrp<-mdfr[mdfr$case==case,];
        if (nrow(mdfrp)>0){
            p <- ggplot(mdfrp,aes_string(y='zp',x='z',size='val',fill='val'));
            p <- p + geom_abline(slope=1,colour='black',linetype=2)
            p <- p + geom_point(alpha=0.8,shape=21) + scale_size_area(max_size=10)
            p <- p + scale_fill_gradientn(colours=wtsUtilities::createColorPalette('jet',100,alpha=0.7))
            p <- p + labs(x="pre-molt size (mm CW)", y="post-molt size (mm CW)");
            p <- p + guides(size=guide_legend("probability",order=1));
            p <- p + guides(fill=guide_colorbar("probability",alpha=1.0,order=2));
            if (length(cases)==1){
                p <- p + facet_grid(x~.);
            } else {
                p <- p + facet_grid(x~case);
            }
            p = p + std_theme;
            if (showPlot) print(p);

            cap<-paste("  \n  \nFigure &&figno. Estimated growth matrices, as bubble plots, for scenario ",case,". \n \n",sep='');
            plots[[cap]]<-p;
        }
    }

    if (verbose) message("Finished rCompTCMs::compareResults.Pop.GrowthMatrices().\n")
    return(plots);
}
