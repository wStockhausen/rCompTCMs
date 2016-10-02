#'
#'@title Compare population quantities from TCSAM2013 and TCSAM02 model runs
#'
#'@description Function to compare population quantities from TCSAM2013 and TCSAM02 model runs.
#'
#'@param objs -  (named list of objects)
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
comparePopQuants<-function(objs,
                           showPlot=TRUE,
                           pdf=NULL,
                           width=8,
                           height=6,
                           verbose=FALSE){
  
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close())
    }
  
    cases<-names(objs);
    
    plots<-list();

    #----------------------------------
    #recruitment
    #----------------------------------
    if (verbose) cat("Plotting recruitment\n");
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.PopQuants(obj,type="R_y");
        if (inherits(obj,"rsimTCSAM"))        mdfr1<-rsimTCSAM::getMDFR.PopQuantities(obj,type="R_y");
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.PopQuantities(obj,verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    p<-rTCSAM2015::plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                   colour='model',guideTitleColor='',
                   shape='model',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$R_y<-p;
    p<-rTCSAM2015::plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   colour='model',guideTitleColor='',
                   shape='model',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$lnR_y<-p;

    #----------------------------------
    #initial size distribution
    #----------------------------------
    if (verbose) cat("Plotting initial size distribution\n");
    mdfr1<-rTCSAM2013::getMDFR.PopQuants(reps2013,type="iN_xmsz");
    mdfr2<-rTCSAM2015::getMDFR('mr/iN_xmsz',reps2015,rsims);
    mdfr2$y<-NA;
    mdfr<-rbind(mdfr1,mdfr2);
    mdfr<-rTCSAM2015::removeImmOS(mdfr);
    p<-rTCSAM2015::plotMDFR.XY(mdfr,x='z',agg.formula='model+x+m+s+z',faceting='m+s~x',
                          xlab='size (mm CW)',ylab='Initial size distribution',units='millions',lnscale=FALSE,
                          colour='model',guideTitleColour='Model\nCase',
                          shape=NULL,guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$iN_xmsz<-p;
    
    return(plots);
}
