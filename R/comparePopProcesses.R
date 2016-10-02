#'
#'@title Compare population processes from TCSAM2013 and TCSAM02 model runs
#'
#'@description Function to compare population processes from TCSAM2013 and TCSAM02 model runs.
#'
#'@param objs
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print debug info
#'
#'@return list of ggplot2 objects
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
comparePopProcesses<-function(objs,
                              showPlot=TRUE,
                              pdf=NULL,
                              width=8,
                              height=6,
                              verbose=FALSE){
  
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    cases<-names(objs);
    
    plots<-list();

    #----------------------------------
    #natural mortality
    #----------------------------------
    if (verbose) cat("Plotting natural mortality info\n");
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.NaturalMortality(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.NaturalMortality(obj,'M_yxm',verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.NaturalMortality(obj,'M_yxm',verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y)
    p <- ggplot(mdfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + ylim(c(0,NA))
    p <- p + facet_grid(x~m);
    p <- p + labs(x="", y="M");
    if (showPlot||!is.null(pdf)) print(p);
    plots$M_yxm<-p;
    
    # #----------------------------------
    # #pr(molt-to-maturity|z)
    # #----------------------------------
    # mdfr1<-rTCSAM2013::getMDFR.PopProcesses(reps2013,type="prM2M_cxz",verbose);
    # mdfr2<-rTCSAM2015::getMDFR.prM2M(reps2015,rsims,verbose);
    # mdfr<-rbind(mdfr1,mdfr2);
    # p<-rTCSAM2015::plotMDFR.XY(mdfr,x='z',agg.formula='pc+x+model+z',faceting='pc~x',
    #                            colour='model',guideTitleColour='',
    #                            shape='model',guideTitleShape='',
    #                            xlab='size (mm CW)',ylab='pr(molt-to-maturity)');
    # if (showPlot||!is.null(pdf)) print(p);
    # plots$prM2M_cz<-p;
    # 
    # #mean growth increments
    # if (verbose) cat("Plotting mean growth increments\n");
    # mdfr1<-rTCSAM2013::getMDFR.PopProcesses(reps2013,type="mnZAM_cxz",verbose);
    # mdfr2<-rTCSAM2015::getMDFR.MeanGrowthIncrements(reps2015,rsims,verbose);
    # mdfr<-rbind(mdfr1,mdfr2);
    # p<-rTCSAM2015::plotMDFR.XY(mdfr,x='z',value.var='val',faceting='pc~x',
    #                            plotABline=TRUE,
    #                            xlab='pre-molt size (mm CW)',ylab='post-molt size (mm CW)',units="",
    #                            shape='model',guideTitleShape='',
    #                            colour='model',guideTitleColour='');
    # if (showPlot||!is.null(pdf)) print(p);
    # plots$mnZAM_cz<-p;
    # 
    # #growth transition matrices
    # if (verbose) cat("Plotting growth transition matrices\n");
    # mdfr1<-rTCSAM2013::getMDFR.PopProcesses(reps2013,type="T_cxzz",verbose);
    # mdfr1$z<-floor(mdfr1$z)+0.5;
    # mdfr2<-rTCSAM2015::getMDFR.GrowthTransitionMatrices(reps2015,rsims,verbose);
    # mdfr<-rbind(mdfr1,mdfr2);
    # p<-rTCSAM2015::compareModels.GrowthTransitionMatrices(mdfr);
    # if (showPlot||!is.null(pdf)) print(p);
    # plots$T_czz<-p;
    # 
    # #----------------------------------
    # #recruitment size distribution
    # #----------------------------------
    # if (verbose) cat("Plotting recruitment size distribution\n");
    # mdfr1<-rTCSAM2013::getMDFR.PopProcesses(reps2013,type="R_cz")
    # mdfr2<-rTCSAM2015::getMDFR.RecSizeDistribution(reps2015,rsims,verbose);
    # mdfr<-rbind(mdfr1,mdfr2);
    # p<-rTCSAM2015::plotMDFR.XY(mdfr,x='z',agg.formula='pc+x+model+z',faceting='pc~.',
    #                xlab='size (mm CW)',ylab='Recruitment size distribution',units='millions',lnscale=FALSE,
    #                colour='model',guideTitleColour='',
    #                shape='model',guideTitleShape='');
    # if (showPlot||!is.null(pdf)) print(p);
    # plots$R_cz<-p;
    
    return(plots);
}
