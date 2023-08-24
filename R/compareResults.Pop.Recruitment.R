#'
#'@title Function to compare recruitment estimates by year among several models
#'
#'@description This function compares recruitment estimates by year
#'   among several models.
#'
#'@param objs - list of resLst objects or dataframe from \code{extractMDFR.Pop.Recruitment}
#'@param numRecent - number of "recent" years to plot
#'@param dodge - width to dodge overlapping series
#' @param plotPoints - flag to include points (default: FALSE)
#' @param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#' @param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return list of ggplot2 objects
#'
#'@details None.
#'
#'@export
#'
compareResults.Pop.Recruitment<-function(objs,
                                         numRecent=15,
                                          dodge=0.2,
                                            plotPoints=FALSE,
                                            colour_scale=NULL,
                                            fill_scale=NULL,
                                          showPlot=FALSE,
                                          pdf=NULL,
                                          verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Pop.Recruitment().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (inherits(objs,"data.frame")){
        mdfr<-objs;
    } else if (class(objs)=="list"){
        mdfr<-extractMDFR.Pop.Recruitment(objs,verbose=verbose);
    } else {
        msg <- paste0("Error in compareResults.Pop.Recruitment: 'objs' should be a list or inherit from data.frame, \n",
                      "but it was of class ",class(objs),"\n");
        stop(msg);
    }

    idx<-mdfr$y>=(max(mdfr$y)-numRecent);

    #----------------------------------
    #recruitment
    #----------------------------------
    plots<-list();
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                   dodge=dodge,
                   plotPoints=plotPoints,
                   colour_scale=colour_scale,
                   fill_scale=fill_scale,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated annual recruitment.  \n  \n";
    plots[[cap1]]<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                   dodge=dodge,
                   plotPoints=plotPoints,
                   colour_scale=colour_scale,
                   fill_scale=fill_scale,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated recent recruitment.  \n  \n";
    plots[[cap1]]<-p;

    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   dodge=dodge,
                   plotPoints=plotPoints,
                   colour_scale=colour_scale,
                   fill_scale=fill_scale,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated annual recruitment, on ln-scale.  \n  \n";
    plots[[cap1]]<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   dodge=dodge,
                   plotPoints=plotPoints,
                   colour_scale=colour_scale,
                   fill_scale=fill_scale,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated recent recruitment, on ln-scale.  \n  \n";
    plots[[cap1]]<-p;

    if (verbose) cat("rCompTCMs::compareResults.Pop.Recruitment: Done!\n");
    return(plots)
}
