#'
#'@title Compare population quantities from TCSAM2013, TCSAM02, and rsimTCSAM model runs
#'
#'@description Function to compare population quantities from TCSAM2013, TCSAM02, and rsimTCSAM model runs.
#'
#'@param objs -  (named list of objects)
#' @param plotPoints - flag to include points (default: FALSE)
#' @param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#' @param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
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
                                        plotPoints=FALSE,
                                        colour_scale=NULL,
                                        fill_scale=NULL,
                                        showPlot=TRUE,
                                        pdf=NULL,
                                        width=8,
                                        height=6,
                                        verbose=FALSE){
    options(stringsAsFactors=FALSE);

    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    cases<-names(objs);

    plots<-list();

    #----------------------------------
    #recruitment
    #----------------------------------
    if (verbose) cat("Plotting recruitment\n");
    ps<-compareResults.Pop.Recruitment(objs,
                                       dodge=0.2,
                                       plotPoints=plotPoints,
                                       colour_scale=colour_scale,
                                       fill_scale=fill_scale,
                                       showPlot=FALSE,
                                       verbose=verbose);
    if (showPlot||!is.null(pdf)) print(ps);
    plots$R_y<-ps[[1]];
    plots$lnR_y<-ps[[2]];

    #----------------------------------
    #mature biomass
    #----------------------------------
    if (verbose) cat("Plotting mature biomass\n");
    ps<-compareResults.Pop.MatureBiomass(objs,
                                         dodge=0.2,
                                         plotPoints=plotPoints,
                                         colour_scale=colour_scale,
                                         fill_scale=fill_scale,
                                         showPlot=FALSE,
                                         verbose=verbose);
    if (showPlot||!is.null(pdf)) print(ps);
    plots$MB_yx<-ps[[1]];
    plots$lnMB_yx<-ps[[2]];

    #----------------------------------
    #initial size distribution
    #----------------------------------
    if (verbose) cat("Plotting initial size distribution\n");
    p<-compareResults.Pop.IF.NatZ(objs,type="iN_xmsz",verbose=verbose);
    if (showPlot||!is.null(pdf)) print(p);
    plots$iN_xmsz<-p;

    #----------------------------------
    #final size distribution
    #----------------------------------
    if (verbose) cat("Plotting final size distribution\n");
    p<-compareResults.Pop.IF.NatZ(objs,type="fN_xmsz",verbose=verbose);
    if (showPlot||!is.null(pdf)) print(p);
    plots$fN_xmsz<-p;

    return(plots);
}
