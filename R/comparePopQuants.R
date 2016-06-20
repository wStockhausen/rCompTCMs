#'
#'@title Compare population quantities from TCSAM2013 and 2015 model runs
#'
#'@description Function to compare population quantities from TCSAM2013 and 2015 model runs.
#'
#'@param reps2013 - TCSAM2013 report file object (or named list of objects)
#'@param reps2015 - TCSAM2015 report file object (or named list of objects)
#'@param rsims    - rsimTCSAM results object (named list of objects)
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
comparePopQuants<-function(reps2013=NULL,
                           reps2015=NULL,
                           rsims=NULL,
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
    
    if (inherits(reps2013,'tcsam2013.rep')){
        reps2013<-list(`2013`=reps2013);#wrap in list
    }
    if (inherits(reps2015,'tcsam2015.rep')){
        reps2015<-list(`2015`=reps2015);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    plots<-list();

    #----------------------------------
    #recruitment
    #----------------------------------
    if (verbose) cat("Plotting recruitment\n");
    mdfr1<-rTCSAM2013::getMDFR.PopQuants(reps2013,type="R_y")
    mdfr2<-rTCSAM2015::getMDFR('mp/R_list/R_y',reps2015,rsims);
    mdfr<-rbind(mdfr1,mdfr2);
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
