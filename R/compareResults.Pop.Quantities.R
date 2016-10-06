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
compareResults.Pop.Quantities<-function(objs,
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
    ps<-compareResults.Pop.Recruitment(objs,dodge=0.2,showPlot=FALSE,verbose=verbose);
    if (showPlot||!is.null(pdf)) print(ps);
    plots$R_y<-ps[[1]];
    plots$lnR_y<-ps[[2]];

    #----------------------------------
    #mature biomass
    #----------------------------------
    if (verbose) cat("Plotting mature biomass\n");
    ps<-compareResults.Pop.MatureBiomass(objs,dodge=0.2,showPlot=FALSE,verbose=verbose);
    if (showPlot||!is.null(pdf)) print(ps);
    plots$MB_yx<-ps[[1]];
    plots$lnMB_yx<-ps[[2]];

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
